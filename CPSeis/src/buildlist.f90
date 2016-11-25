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
! Name       : buildlist
! Category   : cfe
! Written    : 2000-08-15   by: Donna K. Vunderink
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Multi-Workfile Builder Parameter Screen Module.
! Portability: No known limitations, but see note below.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
! The general operation of this screen is to generate a list of text values
! (which can be done several different ways), indicate where the list values are
! to be inserted in the Value For Each Job list and then execute the insertion.
! This can be done repeatedly inserting different lists into different parts of
! the Value For Each Job list.
!
! (All list names must be in UPPER CASE even though they may be associated with
! list files with lower case filenames.)
!
!
! Building and Editing Lists
! --------------------------
!
! A list is an array of text values with a name, called the list name,
! associated with it.  Lists can be stored in list files which are text files
! with a single column of values whose filename is the same as the list name
! but with a .lst extension.  To use a list in this screen, it must be entered
! into the list buffer.  The following actions allow you to control the list
! buffer.
!
!     Read List    Press the Read List button to access the Read List dialog
!                  box which allows you to read into the buffer a list
!                  stored as a list file.
!
!     Remove List  Press the Remove List button to remove the currently
!                  displayed list from the list buffer.
!
!     Write List   Press the Write List button to write the currently displayed
!                  list values to a list file in the current directory.
!                  A .lst extension will be used for the filename.
!
!     Clear Buffer Press the Clear Buffer button to remove the contents of the
!                  list buffer.
!
!     Shift Left   Press the Shift Left button "<<" to display the previous
!                  list in the list buffer.
!
!     Shift Right  Press the Shift Right button ">>" to display the next list
!                  in the list buffer.
!
! There are several ways of entering lists into the list buffer.
!
!     1.  Press the Read List button to read in an existing list file.  (You
!     can build the list file with a text editor.)
!
!     2.  Press the Select Spread Sheet button to bring up the Select Spread
!     Sheet dialog box.  This allows the user to select a Spread Sheet
!     file, which is a special text file format that can contain multiple
!     columns of values, each with its own name.  Spread Sheet files must have
!     a .sst extension.  You can easily write spread sheet files with Excel,
!     using the text file output option.
!
!     Each Spread Sheet column is loaded into the list buffer as a separate
!     list using the (uppercase) column name as the list name.
!
!     3.  Use the New List tool, located in the middle of the Build Jobnames
!     screen.  (Details are given below in the section titled "Using the
!     New List Tool".)
!
!     4.  Write the desired list name in the List Name field and enter the
!     desired list values in the List Values scrollable list.
!
!     5.  Use any of the preceding methods to enter a list and manually modify
!     the values displayed in the List Values scrollable list.
!
!
! Using the New List Tool
! -----------------------
!
! The New List tool allows you to conveniently generate lists consisting of
! either numeric or alphabetic elements.
!
!     1.  Enter the name for your new list in the List name text field.
!
!     2.  Set the entry in the Starting Index field to the list index number
!     where you want the new list values to start.  Set the entry in the List
!     Increment field to the desired index increment for list values.
!     (Normally this will be 1.)  Set the entry in the Total to Do field to the
!     number of values you want in your list.
!
!     3.  Enter the first value of your new list in the Starting Value field.
!
!     (Leading zero placeholders will be dropped as necessary when incrementing.
!     For example:  0099 incrementing by 1 goes to 0100 and 04 incrementing by
!     8 goes to 12.)
!
!     4.  Enter, in the Duplicate field, the number of times you want a
!     value duplicated before it increments.
!
!     5.  Enter the amount you want your value to increment in the Increment
!     field (this must be a positive integer).  If the Starting Value is
!     alphabetic, the Increment value increments the ASCII code of the alpha
!     character.
!
!     6.  Enter the last value you want in your list in the Ending Value field
!     and the #To Do field entry will be automatically calculated,
!
!                                    or
!
!     Enter the number of values you want in your list (in each cycle) in
!     the #To Do field and the Ending Value field entry will be automatically
!     calculated.
!
!     7.  Enter in the # Cycles field the number of times you want the list
!     values you have specified thus far to repeat.  This is the number of
!     times you want your list to cycle.
!
!     8.  Press the Execute button on the New List tool and the specified new
!     list will be generated and read into the list buffer.  The Undo button
!     on the New List tool removes the last new list execute action.
!
!
! Updating the Value For Each Job List
! ------------------------------------
!
! The Substitution text field contains the value from the template workfile as
! a default.  The List Values scrollable list (on the left side of the screen)
! contains values which can be inserted in the Value For Each Job list.  The 
! following steps describe how to do this.
!
!     1.  Accept the default entry in the Substitution field or modify it
!     manually.
!
!     2.  Determine what list you wish to insert in the Value For Each Job
!     list.
!
!     3.  Highlight a portion of the Substitution field entry and key in the
!     desired list name with a "$" prepended.  That is, key in "$MYLIST" if
!     you want to insert the values in the list named "MYLIST".  (The list need
!     not be displayed in the List Values scrollable list, but must be included
!     in the List Buffer.)
!
!     You can highlight any part of the Substitution field entry: the
!     beginning, the end or in the middle.
!
!     4.  Set the entry in the Starting Index field to the index of the Value
!     For Each Job list where you want the list substitution to start.  Set the
!     entry in the Job Increment field to the increment value for list
!     substitution if you do not want the list values to substitute into
!     consecutive workfiles.  (Otherwise it should be 1.)
!
!     5.  Press the Execute button to make the substitution.  When you do this
!     the list values are substituted and the current parameter values are
!     shown in the Value for Each Job scrollable list at the right side of the
!     screen.  The Undo button removes the last execute action.
!
!     6.  Now redo steps 1-5 to make an additional insertion.
!
!
! When Building Jobnames
! ----------------------
!
! NOTE: ALL JOBNAMES MUST START WITH AN ALPHA CHARACTER, HAVE NO MORE THAN 15
! CHARACTERS AND CONSIST OF ALPHA CHARACTERS, NUMERIC CHARACTERS AND UNDERBARS
! "_" ONLY.
!
! Press OK or Apply to accept the jobnames in the Value For Each Job list.
!
!
! When Building Parameter Values
! ------------------------------
!
! Press the Run Traps button to run traps for the current parameters you have
! specified.  (This is optional and can take some time, but running traps now
! can catch problems early.)
!
! Press OK, Apply or (if present) select another parameter using the 
! Process#Parameter combo box to accept the parameter values in the Value For
! Each Job list.
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
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!023. 2006-09-18  D. Glover    Added NULLIFY statements for Intel compiler.
! 22. 2006-01-10  B. Menger    Removed Unused Variables.
! 21. 2003-11-18  SMCook       Removed gui_def section and moved contents to
!                               the programming_doc section.  This primitive
!                               does not satisfactorily generate its own
!                               buildlist.xml file -- it intentionally uses
!                               hand-coded, stored xml.  Having a gui_def
!                               section in this file can cause the stored xml
!                               to be accidentally overwritten.
! 20. 2003-10-03  Stoeckley    Provide workarounds for Portland Group compiler.
! 19. 2003-09-15  Stoeckley    Changed type to primitive; changed category to
!                               cfe; changed names of called primitives as
!                               necessary.
! 18. 2002-09-23  Vunderink    Changed prints to writes for checkc.
! 17. 2002-08-28  Vunderink    Fixed buildlist_set_values to not adjust length
!                                of jobValueList.
! 16. 2002-03-07  Vunderink    Remove file lock from write opens of files.
! 15. 2002-01-04  Vunderink    Changed the name of this module from mwb_param
!                                to buildlist and enhanced it to support the
!                                buildlist tool.
! 14. 2001-08-16  Vunderink    Allow list to have extension other than .lst
! 13. 2001-06-25  Vunderink    Changed filebox keywords.
! 12. 2001-02-20  Vunderink    Made sure there was a return after each pc_error.
! 11. 2001-01-09  Vunderink    Fixed problem reading list from directory other
!                                than working_dir.
! 10. 2001-01-07  Vunderink    Cleared the listName and listValues if buffer is
!                                empty, fix bug in jobInc trap, allow negative
!                                values for incListinc and fix bug in clearing
!                                buffer.
!  9. 2000-12-19  Vunderink    Added documentation and display list if name
!                                already in list buffer and made substitutions
!                                start at the end of the list buffer now that
!                                it's in alphabetical order. 
!  8. 2000-12-11  Vunderink    Added incCycles and inListTodo fields and
!                                support for the reset button
!  7. 2000-12-06  Vunderink    Made list extention .lst and spreadsheet
!                                extention .sst
!  6. 2000-12-05  Vunderink    Added spreadsheet and clear buttons.
!  5. 2000-10-20  Vunderink    Fixed maxdo calculation
!  4. 2000-10-20  Vunderink    Fixed problem of changes not saved in list
!  3. 2000-10-20  Vunderink    Added incUndo button, fixed writing lists, fixed
!                                to save lists
!  2. 2000-10-16  Vunderink    Screen redesign.
!  1. 2000-08-15  Vunderink    Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! The Portland Group compiler incorrectly executes code such as the following:
!
!       subroutine glotch_get_next (aaa,next)
!       type(xxxx_struct),pointer :: aaa,next
!       next => aaa%next
!       end subroutine
!
!       call glotch_get_next (ccc,ccc)
!
! To work around the bug, calls such as the above (in this file and several
! others) have been changed to:
!
!       call glotch_get_next (ccc,next)
!       ccc => next
!
! Changing the subroutine like this does not fix the problem:
!
!       subroutine glotch_get_next (aaa,next)
!       type(xxxx_struct),pointer :: aaa,next
!       type(xxxx_struct),pointer :: temp
!       temp => aaa%next
!       next => temp
!       end subroutine
!
! The Portland Group compiler required modifications in the code as follows:
!
!         Replaced reverse INDEX intrinsic function calls with STRING_INDEX.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
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
!<NS MBParameterScreen>
!
!   jobStart`P
!   jobInc`P
!   listSubstitution`P
!   selectSpreadsheet`P
!   readList`P
!   writeList`P
!   clearList`P
!   clearBuffer`P
!   shiftListLeft`P
!   listName`P
!   shiftListRight`P
!   listValues`P
!   incListName`P
!   incListStart`P
!   incListInc`P
!   incListTodo`P
!   incStart`P
!   incDuplicate`P
!   incIncrement`P
!   incCycles`P
!   incEnd`P
!   incTodo`P
!   incExecute`P
!   incUndo`P
!   execute`P
!   undo`P
!   runtraps`P
!   jobValueTemplate`P
!   jobValueList`P
!   OK`P
!   APPLY`P
!   CANCEL`P
!   RESET`P
!   PARAMETERLIST`P
!   HELP`P
!
!-------------------------------------------------------------------------------
!</programming_doc>
!
!<HelpSection>
!
!<Help KEYWORD="jobStart">
!<Tip> Initial index to use in applying the substitution. </Tip>
! Text field showing the initial index to use in applying the substitution.
! (This index pertains to the list of workfiles being built.)
!</Help>
!
!<Help KEYWORD="jobInc">
!<Tip> Index increment to use in applying the substitution. </Tip>
! Text field showing the index increment to use in applying the substitution.
! (This index pertains to the list of workfiles being built.)
!</Help>
!
!<Help KEYWORD="listSubstitution">
!<Tip> Field showing the default text used in applying the substitution. </Tip>
! Text field showing the default text used in applying the substitution.  You
! can modify the default text manually and/or by list substitution.
!</Help>
!
!<Help KEYWORD="selectSpreadsheet">
!<Tip> Button that accesses the Select Spreadsheet dialog box. </Tip>
! The Select Spreadsheet dialog box allows you to read lists from a spread 
! sheet file into the list buffer.
!</Help>
!
!<Help KEYWORD="readList">
!<Tip> Button that accesses the Read List dialog box. </Tip>
! The Read List dialog box allows you to read lists from list files into the 
! list buffer.  List files are text files with a single column of values whose
! filename is the same as the list name but with a .lst extension.
!</Help>
!
!<Help KEYWORD="writeList">
!<Tip> Button that writes the displayed list to a list file. </Tip>
! Button that writes the displayed list to a list file in your current 
! directory.
!</Help>
!
!<Help KEYWORD="clearList">
!<Tip> Button that removes the displayed list from the list buffer. </Tip>
!
!</Help>
!
!<Help KEYWORD="clearBuffer">
!<Tip> Button that removes all lists from the list buffer. </Tip>
!
!</Help>
!
!<Help KEYWORD="shiftListLeft">
!<Tip> Button that displays the previous list in the list buffer. </Tip>
!
!</Help>
!
!<Help KEYWORD="listName">
!<Tip> Text field showing the name of the selected/displayed list. </Tip>
! Entering the name of a list in this field causes that list to be displayed in
! the List Values scrollable list.
!</Help>
!
!<Help KEYWORD="shiftListRight">
!<Tip> Button that displays the next list in the list buffer. </Tip>
!
!</Help>
!
!<Help KEYWORD="listValues">
!<Tip> Scrollable list of values in the selected/displayed list. </Tip>
!
!</Help>
!
!<Help KEYWORD="incListName">
!<Tip> Text field showing the name to use for the new list. </Tip>
! Text field showing the name to use for the new list in the New List tool.
!</Help>
!
!<Help KEYWORD="incListStart">
!<Tip> Text field showing the starting list index for new list values. </Tip>
! Text field showing the index number where new list values should start.
!</Help>
!
!<Help KEYWORD="incListInc">
!<Tip> Text field showing the index increment to use in applying values. </Tip>
! Text field showing the index increment to use in applying list values.
!</Help>
!
!<Help KEYWORD="incListTodo">
!<Tip> Text field showing the total number of values to build. </Tip>
! Text field showing the total number of values you want in your new list.
!</Help>
!
!<Help KEYWORD="incStart">
!<Tip> Text field showing the initial value for your new list. </Tip>
!
! Leading zero placeholders will be dropped as necessary when incrementing.
! For example:  0099 incrementing by 1 goes to 0100 and 04 incrementing by
! 8 goes to 12.
!</Help>
!
!<Help KEYWORD="incDuplicate">
!<Tip> Text field showing the number of times to duplicate a list value. </Tip>
! Text field showing the number of times you want a list value duplicated 
! before it increments.
!</Help>
!
!<Help KEYWORD="incIncrement">
!<Tip> Text field showing the amount to increment successive list values. </Tip>
! This must be a positive integer.  
!
! If the Starting Value is alphabetic, the Increment value increments the 
! ASCII code of the alphabetic character.
!</Help>
!
!<Help KEYWORD="incCycles">
!<Tip> Field showing the number of times the list values should repeat. </Tip>
! Text field showing the number of times the list values you have specified 
! thus far should repeat.  This is the number of times you want your list to
! cycle.
!
! Each cycle will have the number of values shown in the #To Do field.
!</Help>
!
!<Help KEYWORD="incEnd">
!<Tip> Text field showing the last value you want in your list. </Tip>
! Enter the last value you want in your list in the Ending Value field and the
! #To Do field entry will be automatically calculated.
!</Help>
!
!<Help KEYWORD="incTodo">
!<Tip> Text field showing the number of values in your list per cycle. </Tip>
! Enter the number of values you want in your list (per cycle) in the #To Do 
! field and the Ending Value field entry will be automatically calculated.
!</Help>
!
!<Help KEYWORD="incExecute">
!<Tip> Button that creates the new list using the parameters specified. </Tip>
! Pressing the Execute button in the New List tool causes the specified new 
! list to be generated (or updated) and loaded into the list buffer.
!</Help>
!
!<Help KEYWORD="incUndo">
!<Tip> Button that removes the last new list execute action. </Tip>
!
!</Help>
!
!<Help KEYWORD="execute">
!<Tip> Button to perform list substitution and update the values array. </Tip>
! Pressing the Execute button performs the specified list substitution and 
! updates the entries in the Value for Each Job scrollable list.
!</Help>
!
!<Help KEYWORD="undo">
!<Tip> Button that removes the last execute action. </Tip>
!
!</Help>
!
!<Help KEYWORD="runtraps">
!<Tip> Button that runs all parameter traps on current parameter values. </Tip>
! Pressing the Run Traps button causes all the process parameter traps to be
! run on the current parameter values in the Value for Each Job scrollable list.
!
! This is optional and can take some time.
!</Help>
!
!<Help KEYWORD="jobValueTemplate">
!<Tip> Text field showing value of the parameter in the template workfile.</Tip>
! Text field that shows the value of the substitution parameter in the template
! workfile.
!</Help>
!
!<Help KEYWORD="jobValueList">
!<Tip> List showing values to be used in the multiple workfiles. </Tip>
! Scrolable list showing values to be used in the multiple workfiles.
!</Help>
!
!<Help KEYWORD="OK">
!<Tip> Button that accepts parameter values and removes this screen. </Tip>
! Button that accepts current parameter values and removes this screen.
!</Help>
!
!<Help KEYWORD="APPLY">
!<Tip> Button that accepts current parameter values. </Tip>
! Button that accepts current parameter values, but does not remove this screen.
!</Help>
!
!<Help KEYWORD="CANCEL">
!<Tip> Button that removes this screen without changing parameters. </Tip>
!
!</Help>
!
!<Help KEYWORD="RESET">
!<Tip> Reset parameters to their initial value or last applied value. </Tip>
! Button that resets parameters to their initial value or last applied value.
!</Help>
!
!<Help KEYWORD="PARAMETERLIST">
!<Tip> Combo box to select a parameter from the Changed Parameter list. </Tip>
! Combo box that allows you to select a parameter from the Changed Parameters
! scrollable list.
!
! Choosing a parameter with this combo box selects a parameter from the Changed
! Parameters scrollable list and makes it the current substitution parameter 
! for this screen.
!</Help>
!
!<Help KEYWORD="HELP">
!<Tip> Button that allows the user to access the pop-up Help Window. </Tip>
!
!</Help>
!
!</HelpSection>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module buildlist_module

      use cfeutil_module
      use filebox_module
      use pc_module
      use string_module
      use cio_module
      use path_module
      use finquire_module
      use incutils_module
      use cfelist_module
      use cfelistbuffer_module

      implicit none

      private
      public :: buildlist_create
      public :: buildlist_delete
      public :: buildlist_update
      public :: buildlist_reset
      public :: buildlist_change_dir
      public :: buildlist_set_buffer
      public :: buildlist_set_values
      public :: buildlist_get_name
      public :: buildlist_get_template_value
      public :: buildlist_alloc_valuelist
      public :: buildlist_update_popup

      character(len=100),public,save :: buildlist_ident = &
       '$Id: buildlist.f90,v 1.23 2006/09/18 13:32:38 Glover prod sps $'

      integer,parameter,private   :: STDOUT               = 6


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: buildlist_struct
        private
        character(len=PC_LENGTH)                :: name
        character(len=PC_LENGTH)                :: list_dir
        character(len=PC_LENGTH)                :: jobName
        integer                                 :: jobStart
        integer                                 :: jobInc
        integer                                 :: jobTodo
        logical                                 :: jobLengthFixed
        character(len=PC_LENGTH)                :: listSubstitution
        character(len=PC_LENGTH)                :: ssName
        character(len=PC_LENGTH),pointer        :: ssList(:)
        integer,pointer                         :: NssList
        character(len=PC_LENGTH)                :: listName
        character(len=PC_LENGTH),pointer        :: listValues(:)
        integer                                 :: NlistValues
        character(len=PC_LENGTH),pointer        :: listValues_temp(:)
        integer                                 :: NlistValues_temp
        character(len=PC_LENGTH)                :: incListName
        integer                                 :: incListStart
        integer                                 :: incListInc
        integer                                 :: incListTodo
        character(len=PC_LENGTH)                :: incStart
        integer                                 :: incDuplicate
        integer                                 :: incIncrement
        integer                                 :: incCycles
        character(len=PC_LENGTH)                :: incEnd
        integer                                 :: incTodo
        logical                                 :: incFavorEnd
        character(len=PC_LENGTH)                :: jobValueTemplate
        character(len=PC_LENGTH),pointer        :: jobValueList(:)
        integer                                 :: NjobValueList
        character(len=PC_LENGTH),pointer        :: jobValueList_temp(:)
        type(filebox_struct),pointer            :: ss_dialog
        type(filebox_struct),pointer            :: list_dialog
        type(cfelist_struct),pointer           :: listDisplayed
        type(cfelistbuffer_struct),pointer         :: listBuffer
      end type buildlist_struct


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      type(buildlist_struct),pointer,save :: parobj         ! needed for traps

      contains


!!-------------------------------- create ----------------------------------!!
!!-------------------------------- create ----------------------------------!!
!!-------------------------------- create ----------------------------------!!


      subroutine buildlist_create (obj,dir,numjobs,keyword,value,maxfixed)
      implicit none
      type(buildlist_struct),pointer     :: obj                      !argument
      character(len=*),intent(in)        :: dir                      !argument
      integer         ,intent(in)        :: numjobs                  !argument
      character(len=*),intent(in)        :: keyword                  !argument
      character(len=*),intent(in)        :: value                    !argument
      logical                            :: maxfixed                 !argument

      integer                            :: i                        !local

      allocate (obj)

      nullify (obj%NssList) ! jpa
      nullify (obj%ssList) ! jpa
      nullify (obj%listValues)
      nullify (obj%listValues_temp)
      nullify (obj%jobValueList)
      nullify (obj%ss_dialog)
      nullify (obj%list_dialog)
      nullify (obj%listDisplayed)
      nullify (obj%listBuffer)

      allocate (obj%listValues(1))
      allocate (obj%jobValueList(numjobs))
      allocate (obj%jobValueList_temp(numjobs))

      obj%name                 = trim(keyword)
      obj%list_dir             = trim(dir)
      obj%jobLengthFixed       = maxfixed
      obj%jobName              = trim(keyword)
      obj%jobStart             = 1
      obj%jobInc               = 1
      obj%jobTodo              = numjobs
      obj%listSubstitution     = trim(value)
      obj%listName             = ' '
      obj%NlistValues          = 0
      obj%NlistValues_temp     = 0
      obj%incListName          = ' '
      obj%incListStart         = 1
      obj%incListInc           = 1
      obj%incListTodo          = numjobs
      obj%incStart             = '1'
      obj%incDuplicate         = 1
      obj%incIncrement         = 1
      obj%incCycles            = 1
      call string_ii2cc (numjobs,obj%incEnd)
      obj%incTodo              = numjobs
      obj%incFavorEnd          = .false.
      obj%jobValueTemplate     = trim(value)
      obj%NjobValueList        = numjobs

      do i=1,obj%NjobValueList
        obj%jobValueList(i) = trim(value)
      enddo
      obj%jobValueList_temp = obj%jobValueList

      call buildlist_update (obj)

      return
      end subroutine buildlist_create


!!-------------------------------- delete ----------------------------------!!
!!-------------------------------- delete ----------------------------------!!
!!-------------------------------- delete ----------------------------------!!


      subroutine buildlist_delete (obj)
      implicit none
      type(buildlist_struct),pointer     :: obj                      !argument

      if (.not. associated(obj)) return

      if (associated(obj%listValues))        deallocate(obj%listValues)
      if (associated(obj%listValues_temp))   deallocate(obj%listValues_temp)
      if (associated(obj%jobValueList))      deallocate(obj%jobValueList)
      if (associated(obj%jobValueList_temp)) deallocate(obj%jobValueList_temp)

      deallocate(obj)

      return
      end subroutine buildlist_delete


!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!
!!------------------------------- update -----------------------------------!!


      subroutine buildlist_update (obj)
      implicit none
      type(buildlist_struct),target      :: obj                      !argument

      character(len=PC_LENGTH)           :: ctemp                    !local
      integer                            :: itemp                    !local

      parobj => obj                                          ! needed for traps.

!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!
!!------------------------- read data cards --------------------------------!!


      if (pc_gui_action_present('SS_SELECTION','WINDOWTYPE')) then
         if (pc_gui_action_present('OK','ButtonPress')) then
           call filebox_selection (obj%ss_dialog,obj%ssName)
           if (len_trim(obj%ssName) .eq. 0) then
             call pc_error ('No spreadsheet chosen')
             return
           endif
           call buildlist_ss_name_trap ('ssName')
         else
           call filebox_update (obj%ss_dialog)
           return
         endif
      endif

      if (pc_gui_action_present('FILE_SELECTION','WINDOWTYPE')) then
         if (pc_gui_action_present('OK','ButtonPress')) then
           ctemp = obj%listName
           call filebox_selection (obj%list_dialog,obj%listName)
           if (len_trim(obj%listName) .eq. 0) then
             call pc_error ('No list chosen')
             obj%listName = ctemp
             return
           endif
           call buildlist_list_name_trap ('listName')
         else
           call filebox_update (obj%list_dialog)
           return
         endif
      endif

      if (pc_gui_action_present('incExecute' ,'ButtonPress')) then
        call buildlist_incexecute_trap ('incExecute')
      endif

      if (pc_gui_action_present('incUndo' ,'ButtonPress')) then
        call buildlist_inc_undo_trap ('incUndo')
      endif

      if (pc_gui_action_present('execute','ButtonPress')) then
        call buildlist_execute_trap ('execute')
      endif

      if (pc_gui_action_present('undo','ButtonPress')) then
        call buildlist_undo_trap ('undo')
      endif

      if (pc_gui_action_present('selectSpreadsheet','ButtonPress')) then
        call buildlist_select_ss_trap ('selectSpreadsheet')
        return
      endif

      if (pc_gui_action_present('readList','ButtonPress')) then
        call buildlist_read_list_trap ('readList')
        return
      endif

      if (pc_gui_action_present('writeList','ButtonPress')) then
        call buildlist_write_list_trap ('writeList')
        return
      endif

      if (pc_gui_action_present('clearList','ButtonPress')) then
        call buildlist_clear_list_trap ('clearList')
      endif

      if (pc_gui_action_present('clearBuffer','ButtonPress')) then
        call buildlist_clear_buffer_trap ('clearBuffer')
      endif

      if (pc_gui_action_present('shiftListLeft','ButtonPress')) then
        call buildlist_list_left_trap ('shiftListLeft')
      endif

      if (pc_gui_action_present('shiftListRight','ButtonPress')) then
        call buildlist_list_right_trap ('shiftListRight')
      endif

      if (pc_gui_action_present('jobClearList','ButtonPress')) then
        call buildlist_job_clear_list_trap ('jobClearList')
      endif

      if (pc_gui_action_present('jobWriteList','ButtonPress')) then
        call buildlist_job_write_list_trap ('jobWriteList')
        return
      endif

      ctemp = obj%jobName
      call pc_get ('jobName'         ,obj%jobName     ,buildlist_job_name_trap)
      if (pc_update_error()) obj%jobName = ctemp

      itemp = obj%jobStart
      call pc_get ('jobStart'        ,obj%jobStart    ,buildlist_job_start_trap)
      if (pc_update_error()) obj%jobStart = itemp

      itemp = obj%jobInc
      call pc_get ('jobInc'          ,obj%jobInc      ,buildlist_job_inc_trap  )
      if (pc_update_error()) obj%jobInc = itemp

      itemp = obj%jobTodo
      call pc_get ('jobTodo'         ,obj%jobTodo     ,buildlist_job_todo_trap )
      if (pc_update_error()) obj%jobTodo = itemp

      call pc_get ('listSubstitution',obj%listSubstitution,  &
                                                    buildlist_substitution_trap)
      call pc_get ('listName'        ,obj%listName    ,buildlist_list_name_trap)
      call pc_get ('incListName'     ,obj%incListName ,buildlist_name_trap     )

      itemp = obj%incListStart
      call pc_get ('incListStart'    ,obj%incListStart,  &
                                                      buildlist_list_start_trap)
      if (pc_update_error()) obj%incListStart = itemp

      itemp = obj%incListInc
      call pc_get ('incListInc'      ,obj%incListInc  ,buildlist_list_inc_trap )
      if (pc_update_error()) obj%incListInc = itemp

      itemp = obj%incListTodo
      call pc_get ('incListTodo'     ,obj%incListTodo ,buildlist_list_todo_trap)
      if (pc_update_error()) obj%incListTodo = itemp

      ctemp = obj%incStart
      call pc_get ('incStart'        ,obj%incStart    ,buildlist_start_trap    )
      if (pc_update_error()) obj%incStart = ctemp

      itemp = obj%incDuplicate
      call pc_get ('incDuplicate'    ,obj%incDuplicate,buildlist_duplicate_trap)
      if (pc_update_error()) obj%incDuplicate = itemp

      itemp = obj%incIncrement
      call pc_get ('incIncrement'    ,obj%incIncrement,buildlist_increment_trap)
      if (pc_update_error()) obj%incIncrement = itemp

      itemp = obj%incCycles
      call pc_get ('incCycles'       ,obj%incCycles   ,buildlist_cycles_trap   )
      if (pc_update_error()) obj%incCycles = itemp

      ctemp = obj%incEnd
      call pc_get ('incEnd'          ,obj%incEnd      ,buildlist_end_trap      )
      if (pc_update_error()) obj%incEnd = ctemp

      itemp = obj%incTodo
      call pc_get ('incTodo'         ,obj%incTodo     ,buildlist_todo_trap     )
      if (pc_update_error()) obj%incTodo = itemp

      call pc_alloc ('jobValueList'  ,obj%jobValueList,obj%NjobValueList,  &
                      buildlist_job_values_trap)
      call pc_alloc ('listValues'    ,obj%listValues  ,obj%NlistValues,    &
                      buildlist_list_values_trap)


!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!
!!----------------------- write data cards ---------------------------------!!


      call pc_put_minsize_array ('jobValueList', obj%NjobValueList)
      call pc_put_maxsize_array ('jobValueList', obj%NjobValueList)

      call pc_put ('jobName'             ,obj%jobName             )
      call pc_put ('jobStart'            ,obj%jobStart            )
      call pc_put ('jobInc'              ,obj%jobInc              )
      call pc_put ('jobTodo'             ,obj%jobTodo             )
      call pc_put ('listSubstitution'    ,obj%listSubstitution    )
      call pc_put ('listName'            ,obj%listName            )
      call pc_put ('incListName'         ,obj%incListName         )
      call pc_put ('incListStart'        ,obj%incListStart        )
      call pc_put ('incListInc'          ,obj%incListInc          )
      call pc_put ('incListTodo'         ,obj%incListTodo         )
      call pc_put ('incStart'            ,obj%incStart            )
      call pc_put ('incDuplicate'        ,obj%incDuplicate        )
      call pc_put ('incIncrement'        ,obj%incIncrement        )
      call pc_put ('incCycles'           ,obj%incCycles           )
      call pc_put ('incEnd'              ,obj%incEnd              )
      call pc_put ('incTodo'             ,obj%incTodo             )
      call pc_put ('jobValueTemplate'    ,obj%jobValueTemplate    )
      call pc_put ('jobValueList'        ,obj%jobValueList   ,obj%NjobValueList)
      call pc_put ('listValues'          ,obj%listValues     ,obj%NlistValues  )

      call pc_put_sensitive_field_flag ('jobValueTemplate',.false.)

      return
      end subroutine buildlist_update


!!-------------------------------- reset -----------------------------------!!
!!-------------------------------- reset -----------------------------------!!
!!-------------------------------- reset -----------------------------------!!


      subroutine buildlist_reset (obj,keyword,numjobs,value)
      implicit none
      type(buildlist_struct),pointer     :: obj                      !argument
      character(len=*),intent(in)        :: keyword                  !argument
      integer         ,intent(in)        :: numjobs                  !argument
      character(len=*),intent(in)        :: value                    !argument

      obj%jobName              = trim(keyword)
      obj%jobStart             = 1
      obj%jobInc               = 1
      obj%jobTodo              = numjobs
      obj%listSubstitution     = trim(value)
      if (associated(obj%listValues_temp)) deallocate(obj%listValues_temp)
      obj%NlistValues_temp     = 0
      obj%incListName          = ' '
      obj%incListStart         = 1
      obj%incListInc           = 1
      obj%incListTodo          = numjobs
      obj%incStart             = '1'
      obj%incDuplicate         = 1
      obj%incIncrement         = 1
      obj%incCycles            = 1
      call string_ii2cc (numjobs,obj%incEnd)
      obj%incTodo              = numjobs
      obj%incFavorEnd          = .false.
      obj%jobValueTemplate     = trim(value)
      obj%NjobValueList        = numjobs

      obj%jobValueList_temp = obj%jobValueList

      call buildlist_update (obj)

      return
      end subroutine buildlist_reset


!!----------------------------- change_dir ---------------------------------!!
!!----------------------------- change_dir ---------------------------------!!
!!----------------------------- change_dir ---------------------------------!!


      subroutine buildlist_change_dir (obj,dir)
      implicit none
      type(buildlist_struct) ,pointer            :: obj                !argument
      character(len=*),intent(in)                :: dir                !argument

      if (associated(obj)) then
        obj%list_dir = dir
      endif

      return
      end subroutine buildlist_change_dir


!!----------------------------- set_buffer ---------------------------------!!
!!----------------------------- set_buffer ---------------------------------!!
!!----------------------------- set_buffer ---------------------------------!!


      subroutine buildlist_set_buffer (obj,buffer)
      implicit none
      type(buildlist_struct) ,pointer            :: obj                !argument
      type(cfelistbuffer_struct),pointer         :: buffer             !argument

      if (.not. associated(obj)) return

      if (associated(buffer)) then
        obj%listBuffer => buffer
        call cfelistbuffer_get_first_list (obj%listBuffer    ,obj%listDisplayed)
        if (associated(obj%listDisplayed)) then
          call cfelist_get_name         (obj%listDisplayed ,obj%listName)
          call cfelist_alloc_strings    (obj%listDisplayed ,obj%listValues, &
                                                             obj%NlistValues)
          call pc_put ('listName'   ,obj%listName   )
          call pc_put ('listValues' ,obj%listValues ,obj%NlistValues)
        else
          obj%listName    = ' '
          obj%NlistValues = 0
          if (associated(obj%listValues)) then
            deallocate(obj%listValues)
            allocate(obj%listValues(1))
          endif
        endif
      else
        nullify(obj%listBuffer)
        if (associated(obj%listDisplayed)) then
          call cfelist_delete (obj%listDisplayed)
        endif
        obj%listName    = ' '
        obj%NlistValues = 0
        if (associated(obj%listValues)) then
          deallocate(obj%listValues)
          allocate(obj%listValues(1))
        endif
      endif

      return
      end subroutine buildlist_set_buffer


!!----------------------------- set_values ---------------------------------!!
!!----------------------------- set_values ---------------------------------!!
!!----------------------------- set_values ---------------------------------!!


      subroutine buildlist_set_values (obj,values,nvalues)
      implicit none
      type(buildlist_struct)  ,pointer           :: obj                !argument
      character(len=*)       ,intent(in)         :: values(:)          !argument
      integer                ,intent(in)         :: nvalues            !argument

      integer                                    :: i                  !local


      if (.not. associated(obj)) return

      if (nvalues .lt. obj%NjobValueList) then
        do i=1,nvalues
          obj%jobValueList(i) = trim(values(i))
        enddo
      else
        do i=1,obj%NjobValueList
          obj%jobValueList(i) = trim(values(i))
        enddo
      endif


      call pc_put_minsize_array ('jobValueList',obj%NjobValueList)
      call pc_put_maxsize_array ('jobValueList',obj%NjobValueList)

      call pc_put ('jobValueList',obj%jobValueList,obj%NjobValueList)

      return
      end subroutine buildlist_set_values


!!---------------------------- execute_trap --------------------------------!!
!!---------------------------- execute_trap --------------------------------!!
!!---------------------------- execute_trap --------------------------------!!


      subroutine buildlist_execute_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument


      parobj%jobValueList_temp = parobj%jobValueList

      call buildlist_execute_slist (parobj%listSubstitution,  &
                                    parobj%listBuffer,parobj%jobValueList, &
                                    parobj%NjobValueList,parobj%jobStart,  &
                                    parobj%jobInc)

      return
      end subroutine buildlist_execute_trap


!!------------------------------ undo_trap ---------------------------------!!
!!------------------------------ undo_trap ---------------------------------!!
!!------------------------------ undo_trap ---------------------------------!!


      subroutine buildlist_undo_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument


      parobj%jobValueList = parobj%jobValueList_temp

      return
      end subroutine buildlist_undo_trap


!!--------------------------- select_ss_trap -------------------------------!!
!!--------------------------- select_ss_trap -------------------------------!!
!!--------------------------- select_ss_trap -------------------------------!!


      subroutine buildlist_select_ss_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      character(len=40)               :: filebox_keys(7)               !local
      integer                         :: nfilebox_keys                 !local

      nfilebox_keys = 7
      filebox_keys(1) = 'FILEBOX_FILTER'
      filebox_keys(2) = 'FILEBOX_DIRECTORIES'
      filebox_keys(3) = 'FILEBOX_FILES'
      filebox_keys(4) = 'FILEBOX_SELECTION'
      filebox_keys(5) = 'FILEBOX_FILTERBUTTON'
      filebox_keys(6) = 'OK'
      filebox_keys(7) = 'CANCEL'

      call pc_put_gui ('SS_SELECTION','NewWindow','filebox.xml')
      call pc_put_gui ('SS_SELECTION','WindowType','FILEBOX')
      call pc_put_gui ('SS_SELECTION','Windowkeys',filebox_keys,nfilebox_keys)

      if (.not. associated(parobj%ss_dialog)) then
        call filebox_create (parobj%ss_dialog,'*.sst')
      else
        call filebox_restore (parobj%ss_dialog)
      endif

      return
      end subroutine buildlist_select_ss_trap


!!--------------------------- read_list_trap -------------------------------!!
!!--------------------------- read_list_trap -------------------------------!!
!!--------------------------- read_list_trap -------------------------------!!


      subroutine buildlist_read_list_trap (keyword)
      implicit none
      character(len=*),intent(in)     ::  keyword                      !argument

      character(len=40)               :: filebox_keys(7)               !local
      integer                         :: nfilebox_keys                 !local

      nfilebox_keys = 7
      filebox_keys(1) = 'FILEBOX_FILTER'
      filebox_keys(2) = 'FILEBOX_DIRECTORIES'
      filebox_keys(3) = 'FILEBOX_FILES'
      filebox_keys(4) = 'FILEBOX_SELECTION'
      filebox_keys(5) = 'FILEBOX_FILTERBUTTON'
      filebox_keys(6) = 'OK'
      filebox_keys(7) = 'CANCEL'

      call pc_put_gui ('FILE_SELECTION','NewWindow','filebox.xml')
      call pc_put_gui ('FILE_SELECTION','WindowType','FILEBOX')
      call pc_put_gui ('FILE_SELECTION','Windowkeys',filebox_keys,nfilebox_keys)

      if (.not. associated(parobj%list_dialog)) then
        call filebox_create (parobj%list_dialog,'*.lst')
      else
        call filebox_restore (parobj%list_dialog)
      endif

      return
      end subroutine buildlist_read_list_trap


!!--------------------------- write_list_trap ------------------------------!!
!!--------------------------- write_list_trap ------------------------------!!
!!--------------------------- write_list_trap ------------------------------!!


      subroutine buildlist_write_list_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           :: i                           !local
      integer                           :: lun = 0                     !local
      integer                           :: istat                       !local
      integer                           :: nstring                     !local
      character(len=2)                  :: mode                        !local
      character(len=PC_LENGTH)          :: list_name                   !local
      integer                           :: temp


      if (parobj%NlistValues .eq. 0) then
        call pc_error ('No list values to write')
        return
      endif

      list_name = trim(parobj%listName)//'.lst'
      mode = "w"
      lun = cio_fopen(trim(parobj%list_dir)//trim(list_name),mode, &
                      file_lock=FILE_LOCK_DISABLED)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(parobj%list_dir)//  &
                         trim(list_name)//'  lun=',lun
        return
      endif

      do i=1,parobj%NlistValues
        temp =  len_trim(parobj%listValues(i))
        nstring = cio_fputline(parobj%listValues(i),temp,lun)
        if (nstring .lt. len_trim(parobj%listValues(i))) exit
      enddo

      istat = cio_fclose(lun)

      return
      end subroutine buildlist_write_list_trap


!!--------------------------- clear_list_trap ------------------------------!!
!!--------------------------- clear_list_trap ------------------------------!!
!!--------------------------- clear_list_trap ------------------------------!!


      subroutine buildlist_clear_list_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      if (associated(parobj%listDisplayed)) then
        call cfelistbuffer_delete_list    &
                                (parobj%listBuffer,parobj%listDisplayed)
        call cfelistbuffer_get_first_list &
                                (parobj%listBuffer,parobj%listDisplayed)
        if (associated(parobj%listDisplayed)) then
          call cfelist_get_name      (parobj%listDisplayed, parobj%listName)
          call cfelist_alloc_strings (parobj%listDisplayed, parobj%listValues,&
                                                             parobj%NlistValues)
          if (associated(parobj%listValues_temp))   &
                                              deallocate(parobj%listValues_temp)
          parobj%NlistValues_temp = 0
        else
          parobj%listName = ' '
          parobj%NlistValues = 0
          if (associated(parobj%listValues)) then
            deallocate(parobj%listValues)
            allocate(parobj%listValues(1))
          endif
        endif
      endif

      return
      end subroutine buildlist_clear_list_trap


!!-------------------------- clear_buffer_trap -----------------------------!!
!!-------------------------- clear_buffer_trap -----------------------------!!
!!-------------------------- clear_buffer_trap -----------------------------!!


      subroutine buildlist_clear_buffer_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      if (associated(parobj%listBuffer)) then
        call cfelistbuffer_clear (parobj%listBuffer)
      endif

      nullify(parobj%listDisplayed)
      parobj%listName    = ' '
      parobj%NlistValues = 0
      if (associated(parobj%listValues)) then
        deallocate(parobj%listValues)
        allocate(parobj%listValues(1))
      endif

      return
      end subroutine buildlist_clear_buffer_trap


!!--------------------------- list_left_trap -------------------------------!!
!!--------------------------- list_left_trap -------------------------------!!
!!--------------------------- list_left_trap -------------------------------!!


      subroutine buildlist_list_left_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      type(cfelist_struct),pointer     ::  previous                   !local


      call cfelist_get_previous (parobj%listDisplayed,previous)
      if (associated(previous)) then
        parobj%listDisplayed => previous
        call cfelist_get_name      (parobj%listDisplayed, parobj%listName)
        call cfelist_alloc_strings (parobj%listDisplayed, parobj%listValues, &
                                                           parobj%NlistValues)
        if (associated(parobj%listValues_temp))   &
                                              deallocate(parobj%listValues_temp)
        parobj%NlistValues_temp = 0
      else
        call pc_put_gui (keyword,'Beep',' ')
      endif

      return
      end subroutine buildlist_list_left_trap


!!--------------------------- list_right_trap ------------------------------!!
!!--------------------------- list_right_trap ------------------------------!!
!!--------------------------- list_right_trap ------------------------------!!


      subroutine buildlist_list_right_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      type(cfelist_struct),pointer     ::  next                       !local


      call cfelist_get_next (parobj%listDisplayed,next)
      if (associated(next)) then
        parobj%listDisplayed => next
        call cfelist_get_name      (parobj%listDisplayed, parobj%listName)
        call cfelist_alloc_strings (parobj%listDisplayed, parobj%listValues, &
                                                           parobj%NlistValues)
        if (associated(parobj%listValues_temp))   &
                                              deallocate(parobj%listValues_temp)
        parobj%NlistValues_temp = 0
      else
        call pc_put_gui (keyword,'Beep',' ')
      endif

      return
      end subroutine buildlist_list_right_trap


!!------------------------- job_clear_list_trap ----------------------------!!
!!------------------------- job_clear_list_trap ----------------------------!!
!!------------------------- job_clear_list_trap ----------------------------!!


      subroutine buildlist_job_clear_list_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           ::  i                          !local

      if (associated(parobj%jobValueList) .and. parobj%NjobValueList.gt.0) then
        do i=1,parobj%NjobValueList
          parobj%jobValueList(i) = ' '
        enddo
      endif

      return
      end subroutine buildlist_job_clear_list_trap


!!------------------------- job_write_list_trap ----------------------------!!
!!------------------------- job_write_list_trap ----------------------------!!
!!------------------------- job_write_list_trap ----------------------------!!


      subroutine buildlist_job_write_list_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           :: i                           !local
      integer                           :: lun = 0                     !local
      integer                           :: istat                       !local
      integer                           :: nstring                     !local
      character(len=2)                  :: mode                        !local
      character(len=PC_LENGTH)          :: list_name                   !local
      integer                           :: temp


      if (parobj%NjobValueList .eq. 0) then
        call pc_error ('No list values to write')
        return
      endif

      list_name = trim(parobj%jobName)//'.lst'
      mode = "w"
      lun = cio_fopen(trim(parobj%list_dir)//trim(list_name),mode, &
                      file_lock=FILE_LOCK_DISABLED)
      if (lun .lt. 100) then
        write(STDOUT,*) 'Error opening file '//trim(parobj%list_dir)//  &
                         trim(list_name)//'  lun=',lun
        return
      endif

      do i=1,parobj%NjobValueList
        temp = len_trim(parobj%jobValueList(i))
        nstring = cio_fputline(parobj%jobValueList(i),temp,lun)
        if (nstring .lt. len_trim(parobj%jobValueList(i))) exit
      enddo

      istat = cio_fclose(lun)

      return
      end subroutine buildlist_job_write_list_trap


!!--------------------------- job_name_trap --------------------------------!!
!!--------------------------- job_name_trap --------------------------------!!
!!--------------------------- job_name_trap --------------------------------!!


      subroutine buildlist_job_name_trap (keyword)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument

      integer                           :: i                           !local
      integer                           :: lun = 0                     !local
      integer                           :: istat                       !local
      character(len=2)                  :: mode                        !local
      character(len=PC_DATACARD_LENGTH) :: string                      !local
      integer                           :: nstring                     !local
      character(len=PC_LENGTH)          :: job_name                    !local

      if (len_trim(parobj%jobName) .eq. 0) return

      job_name = parobj%jobName
!!!   i = index        (job_name,'/',.true.)
      i = string_index (job_name,'/',.true.)
      if (i .eq. 0) then
        job_name = trim(parobj%list_dir)//trim(parobj%jobName)
      endif
!!!   i = index        (job_name,'.',.true.)
      i = string_index (job_name,'.',.true.)
      if (i .eq. 0) then
        i = len_trim(job_name)
        job_name(i+1:) = '.lst'
      endif

      istat = finquire_input (trim(job_name))
      if (istat .eq. FINQUIRE_FOUND) then
        mode = "r"
        lun = cio_fopen(trim(job_name),mode)
        if (lun .lt. 100) then
          write(STDOUT,*) 'Error opening file '//trim(job_name)//'  lun=',lun
          call pc_error ('Error opening file '//trim(job_name))
          return
        endif

        if (associated(parobj%jobValueList)) deallocate(parobj%jobValueList)
        do
          nstring = cio_fgetline(string,PC_DATACARD_LENGTH,lun)
          if (nstring .lt. 0) exit
          call cfeutil_append_array_element (parobj%jobValueList,  &
                                             parobj%NjobValueList,trim(string))
        enddo

        istat = cio_fclose(lun)

        parobj%jobTodo     = parobj%NjobValueList
        parobj%incListTodo = parobj%NjobValueList
        parobj%incTodo     = parobj%NjobValueList
        call string_ii2cc (parobj%NjobValueList,parobj%incEnd)

      endif

      return
      end subroutine buildlist_job_name_trap


!!-------------------------- job_start_trap --------------------------------!!
!!-------------------------- job_start_trap --------------------------------!!
!!-------------------------- job_start_trap --------------------------------!!


      subroutine buildlist_job_start_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)          ::  ctemp                      !local


      if (parobj%jobStart.lt.1.or.parobj%jobStart.gt.parobj%NjobValueList)then
        call string_ii2cc (parobj%NjobValueList,ctemp)
        call pc_error ('Start Index must be between 1 and '//trim(ctemp))
        return
      endif

      return
      end subroutine buildlist_job_start_trap


!!--------------------------- job_inc_trap ---------------------------------!!
!!--------------------------- job_inc_trap ---------------------------------!!
!!--------------------------- job_inc_trap ---------------------------------!!


      subroutine buildlist_job_inc_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)          ::  ctemp                      !local


      if (parobj%jobInc.lt.1.or.parobj%jobInc.gt.parobj%NjobValueList) then
        call string_ii2cc (parobj%NjobValueList,ctemp)
        call pc_error ('Increment must be between 1 and '//trim(ctemp))
        return
      endif

      return
      end subroutine buildlist_job_inc_trap


!!--------------------------- job_todo_trap --------------------------------!!
!!--------------------------- job_todo_trap --------------------------------!!
!!--------------------------- job_todo_trap --------------------------------!!


      subroutine buildlist_job_todo_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           ::  maxdo                      !local

      if (parobj%jobTodo .le. 1) then
        call pc_error ('Job Todo must be greater than 1')
        return
      endif

      if (parobj%NjobValueList .lt. parobj%jobTodo) then
        call cfeutil_append_array_element (parobj%jobValueList,       &
                                           parobj%NjobValueList,' ',  &
                                           parobj%jobTodo-parobj%NjobValueList)
      else if (parobj%NjobValueList .gt. parobj%jobTodo) then
        call cfeutil_remove_array_element (parobj%jobValueList,  &
                                           parobj%NjobValueList, &
                                           parobj%jobTodo + 1,   &
                                           parobj%NjobValueList)
      endif

      parobj%incListTodo = parobj%jobTodo
      parobj%incTodo     = parobj%jobTodo

      call incutils_start_trap (parobj%incStart,parobj%incEnd,  &
                                 parobj%incDuplicate,parobj%incIncrement,  &
                                 parobj%incCycles,parobj%incTodo,  &
                                 parobj%incFavorEnd,maxdo)

      return
      end subroutine buildlist_job_todo_trap


!!----------------------------- mask_trap ----------------------------------!!
!!----------------------------- mask_trap ----------------------------------!!
!!----------------------------- mask_trap ----------------------------------!!


      subroutine buildlist_mask_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      return
      end subroutine buildlist_mask_trap


!!------------------------- substitution_trap ------------------------------!!
!!------------------------- substitution_trap ------------------------------!!
!!------------------------- substitution_trap ------------------------------!!


      subroutine buildlist_substitution_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      return
      end subroutine buildlist_substitution_trap


!!---------------------------- ss_name_trap --------------------------------!!
!!---------------------------- ss_name_trap --------------------------------!!
!!---------------------------- ss_name_trap --------------------------------!!


      subroutine buildlist_ss_name_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      call cfeutil_load_spreadsheet (parobj%listBuffer,parobj%ssName) 
      call cfelistbuffer_get_last_list (parobj%listBuffer,parobj%listDisplayed)
      if (associated(parobj%listDisplayed)) then
        call cfelist_get_name      (parobj%listDisplayed,parobj%listName)
        call cfelist_alloc_strings (parobj%listDisplayed,parobj%listValues, &
                                                          parobj%NlistValues)
      endif

      return
      end subroutine buildlist_ss_name_trap


!!--------------------------- list_name_trap -------------------------------!!
!!--------------------------- list_name_trap -------------------------------!!
!!--------------------------- list_name_trap -------------------------------!!


      subroutine buildlist_list_name_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           :: i                           !local
      integer                           :: lun = 0                     !local
      integer                           :: istat                       !local
      character(len=2)                  :: mode                        !local
      character(len=PC_DATACARD_LENGTH) :: string                      !local
      integer                           :: nstring                     !local
      character(len=PC_LENGTH)          :: list_name                   !local
      character(len=PC_LENGTH)          :: current_name                !local
      type(cfelist_struct),pointer     :: current,next                !local

      if (len_trim(parobj%listName) .eq. 0) return

      list_name    = path_get_file (parobj%listName)
!!!   i = index        (list_name,'.',.true.)
      i = string_index (list_name,'.',.true.)
      if (i .gt. 1) then
        list_name = list_name(1:i-1)
      else if (i .eq. 1) then
        call pc_error ('Invalid list name')
        if (associated(parobj%listDisplayed)) then
          call cfelist_get_name      (parobj%listDisplayed, parobj%listName)
          call cfelist_alloc_strings (parobj%listDisplayed, parobj%listValues,&
                                                             parobj%NlistValues)
        else
          parobj%listName = ''
        endif
        return
      endif
      call string_to_upper (list_name)

      if (.not. associated(parobj%listBuffer)) then
        call cfelistbuffer_create         (parobj%listBuffer)
        call cfelist_create           (parobj%listDisplayed)
        call cfelist_set_name         (parobj%listDisplayed,list_name)
!       call cfelist_get_name         (parobj%listDisplayed,parobj%listName)
        call cfelistbuffer_set_first_list &
                                    (parobj%listBuffer,parobj%listDisplayed)
        call cfelistbuffer_set_last_list  &
                                    (parobj%listBuffer,parobj%listDisplayed)
      else
        call cfelistbuffer_get_last_list(parobj%listBuffer,current)
        if (.not. associated(current)) then
          call cfelist_create          (parobj%listDisplayed)
          call cfelist_set_name        (parobj%listDisplayed,list_name)
!         call cfelist_get_name        (parobj%listDisplayed,parobj%listName)
          call cfelistbuffer_set_first_list &
                                    (parobj%listBuffer,parobj%listDisplayed)
          call cfelistbuffer_set_last_list  &
                                    (parobj%listBuffer,parobj%listDisplayed)
        else
          nullify(parobj%listDisplayed)
          call cfelistbuffer_get_first_list (parobj%listBuffer,current)
          do
            if (.not. associated(current)) exit
            call cfelist_get_name (current,current_name)
            if (trim(current_name) .eq. trim(list_name)) then
              parobj%listDisplayed => current
              exit
            endif
!!!         call cfelist_get_next (current,current)
            call cfelist_get_next (current,next)
            current => next
          enddo
          if (.not. associated(parobj%listDisplayed)) then
            call cfelist_create         (parobj%listDisplayed)
            call cfelist_set_name       (parobj%listDisplayed,list_name)
!           call cfelist_get_name       (parobj%listDisplayed,parobj%listName)
            call cfelistbuffer_insert_list  (parobj%listBuffer,    &
                                          parobj%listDisplayed)
          else
            call cfelist_get_name      (parobj%listDisplayed,parobj%listName)
            call cfelist_alloc_strings (parobj%listDisplayed,  &
                                         parobj%listValues,     &
                                         parobj%NlistValues)
            call pc_error ('List name already in buffer')
            return
          endif
        endif
      endif
      current_name = list_name

      list_name    = parobj%listName
!!!   i = index        (list_name,'/',.true.)
      i = string_index (list_name,'/',.true.)
      if (i .eq. 0) then
        list_name = trim(parobj%list_dir)//trim(parobj%listName)
!!!     i = index        (list_name,'.',.true.)
        i = string_index (list_name,'.',.true.)
        if (i .eq. 0) then
          i = len_trim(list_name)
          list_name(i+1:) = '.lst'
        endif
      endif

      istat = finquire_input (trim(list_name))
      if (istat .eq. FINQUIRE_ERROR) then
        if (associated(parobj%listValues)) deallocate(parobj%listValues)
        parobj%NlistValues = parobj%NjobValueList
        allocate(parobj%listValues(parobj%NlistValues))
        parobj%listValues = ''
        call cfelist_set_name    (parobj%listDisplayed,current_name)
        call cfelist_get_name    (parobj%listDisplayed,parobj%listName)
        call cfelist_put_strings (parobj%listDisplayed,parobj%listValues,  &
                                                        parobj%NlistValues)
      else
        mode = "r"
        lun = cio_fopen(trim(list_name),mode)
        if (lun .lt. 100) then
          write(STDOUT,*) 'Error opening file '//trim(list_name)//'  lun=',lun
          call pc_error ('Error opening file '//trim(list_name))
          if (associated(parobj%listDisplayed)) then
            call cfelist_get_name     (parobj%listDisplayed,parobj%listName)
            call cfelist_alloc_strings(parobj%listDisplayed,parobj%listValues,&
                                                             parobj%NlistValues)
          else
            parobj%listName = ''
          endif
          return
        endif

        if (associated(parobj%listValues)) deallocate(parobj%listValues)
        do
          nstring = cio_fgetline(string,PC_DATACARD_LENGTH,lun)
          if (nstring .lt. 0) exit
          call cfeutil_append_array_element (parobj%listValues,  &
                                             parobj%NlistValues,trim(string))
        enddo

        istat = cio_fclose(lun)
        if (.not. associated(parobj%listValues)) allocate(parobj%listValues(1))

        call cfelist_set_name    (parobj%listDisplayed,current_name)
        call cfelist_get_name    (parobj%listDisplayed,parobj%listName)
        call cfelist_put_strings (parobj%listDisplayed,parobj%listValues,  &
                                                        parobj%NlistValues)

      endif

      if (associated(parobj%listValues_temp)) deallocate(parobj%listValues_temp)
      parobj%NlistValues_temp = 0
      
      return
      end subroutine buildlist_list_name_trap


!!----------------------------- name_trap ----------------------------------!!
!!----------------------------- name_trap ----------------------------------!!
!!----------------------------- name_trap ----------------------------------!!


      subroutine buildlist_name_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           ::  i                          !local
      character(len=PC_LENGTH)          ::  listname                   !local
      type(cfelist_struct),pointer     ::  current,next               !local

      if (len_trim(parobj%incListName) .eq. 0) then
        call pc_error ('List Name not specified')
        return
      endif

      if (.not. string_is_alphanum(parobj%incListName(1:1))) then
        call pc_error ('List name must begin with alpha character')
        return
      endif

      call string_to_upper (parobj%incListName)
      do i=1,len_trim(parobj%incListName)
        if (.not. string_is_alphanum(parobj%incListName(i:i))) then
          if (parobj%incListName(i:i) .eq. '_') cycle
          call pc_error ('Only alpha, numeric, and underscore allowed')
          parobj%incListName = ' '
          return
        endif
      enddo

      call cfelistbuffer_get_first_list (parobj%listBuffer,current)
      do
        if (.not. associated(current)) exit
        call cfelist_get_name (current,listname)
        if (trim(parobj%incListName) .eq. trim(listname)) then
          call pc_warning ('The list '//trim(listname)//  &
                           ' already is in the list buffer')
          parobj%listDisplayed => current
          parobj%listName      =  listname
          call cfelist_alloc_strings (parobj%listDisplayed,parobj%listValues, &
                                       parobj%NlistValues)
          if (associated(parobj%listValues_temp))   &
                                             deallocate(parobj%listValues_temp)
          parobj%NlistValues_temp = 0
          exit
        endif
!!!     call cfelist_get_next (current,current)
        call cfelist_get_next (current,next)
        current => next
      enddo

      return
      end subroutine buildlist_name_trap


!!-------------------------- list_start_trap -------------------------------!!
!!-------------------------- list_start_trap -------------------------------!!
!!-------------------------- list_start_trap -------------------------------!!


      subroutine buildlist_list_start_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)          ::  ctemp                      !local
      integer                           ::  maxdo                      !local

      if (parobj%incListStart.lt.1 .or.  &
          parobj%incListStart.gt.parobj%NjobValueList) then
        call string_ii2cc (parobj%NjobValueList,ctemp)
        call pc_error ('List Start Index must be between 1 and '//trim(ctemp))
        return
      endif

      maxdo =(parobj%NjobValueList-parobj%incListStart)/abs(parobj%incListInc)+1
      parobj%incListTodo = maxdo
      call incutils_start_trap (parobj%incStart,parobj%incEnd,  &
                                 parobj%incDuplicate,parobj%incIncrement,  &
                                 parobj%incCycles,parobj%incTodo,  &
                                 parobj%incFavorEnd,maxdo)

      return
      end subroutine buildlist_list_start_trap


!!--------------------------- list_inc_trap --------------------------------!!
!!--------------------------- list_inc_trap --------------------------------!!
!!--------------------------- list_inc_trap --------------------------------!!


      subroutine buildlist_list_inc_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)          ::  ctemp                      !local
      integer                           ::  maxdo                      !local

      if (parobj%incListInc.lt.1 .or.  &
          parobj%incListInc.gt.parobj%NjobValueList) then
        call string_ii2cc (parobj%NjobValueList,ctemp)
        call pc_error ('List Increment must be between 1 and '//trim(ctemp))
        return
      endif

      maxdo =(parobj%NjobValueList-parobj%incListStart)/abs(parobj%incListInc)+1
      parobj%incListTodo = maxdo
      call incutils_start_trap (parobj%incStart,parobj%incEnd,  &
                                 parobj%incDuplicate,parobj%incIncrement,  &
                                 parobj%incCycles,parobj%incTodo,  &
                                 parobj%incFavorEnd,maxdo)

      return
      end subroutine buildlist_list_inc_trap


!!--------------------------- list_todo_trap -------------------------------!!
!!--------------------------- list_todo_trap -------------------------------!!
!!--------------------------- list_todo_trap -------------------------------!!


      subroutine buildlist_list_todo_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)          ::  ctemp                      !local
      integer                           ::  maxdo                      !local

      if (parobj%incListTodo.lt.1 .or.  &
          parobj%incListTodo.gt.parobj%NjobValueList) then
        call string_ii2cc (parobj%NjobValueList,ctemp)
        call pc_error ('List number to do must be between 1 and '//trim(ctemp))
        return
      endif

      maxdo =(parobj%NjobValueList-parobj%incListStart)/abs(parobj%incListInc)+1
      if (parobj%incListTodo.gt.maxdo) then
        parobj%incListTodo = maxdo
      else
        maxdo = parobj%incListTodo
      endif
      call incutils_start_trap (parobj%incStart,parobj%incEnd,  &
                                 parobj%incDuplicate,parobj%incIncrement,  &
                                 parobj%incCycles,parobj%incTodo,  &
                                 parobj%incFavorEnd,maxdo)

      return
      end subroutine buildlist_list_todo_trap


!!----------------------------- start_trap ---------------------------------!!
!!----------------------------- start_trap ---------------------------------!!
!!----------------------------- start_trap ---------------------------------!!


      subroutine buildlist_start_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           ::  maxdo                      !local
      if (parobj%incStart .eq. CNIL) then
        call pc_error ('Start can not be blank')
        return
      endif

      maxdo =(parobj%NjobValueList-parobj%incListStart)/abs(parobj%incListInc)+1
      if (parobj%incListTodo.gt.maxdo) then
        parobj%incListTodo = maxdo
      else
        maxdo = parobj%incListTodo
      endif
      call incutils_start_trap (parobj%incStart,parobj%incEnd,  &
                                 parobj%incDuplicate,parobj%incIncrement,  &
                                 parobj%incCycles,parobj%incTodo,  &
                                 parobj%incFavorEnd,maxdo)
      return
      end subroutine buildlist_start_trap


!!--------------------------- duplicate_trap -------------------------------!!
!!--------------------------- duplicate_trap -------------------------------!!
!!--------------------------- duplicate_trap -------------------------------!!


      subroutine buildlist_duplicate_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           ::  maxdo                      !local

      if (parobj%incDuplicate .le. 0) then
        call pc_error ('Duplicate must be greater than zero')
        return
      endif

      maxdo =(parobj%NjobValueList-parobj%incListStart)/abs(parobj%incListInc)+1
      if (parobj%incListTodo.gt.maxdo) then
        parobj%incListTodo = maxdo
      else
        maxdo = parobj%incListTodo
      endif
      call incutils_dup_trap (parobj%incStart,parobj%incEnd,  &
                               parobj%incDuplicate,parobj%incIncrement,  &
                               parobj%incCycles,parobj%incTodo,  &
                               parobj%incFavorEnd,maxdo)

      return
      end subroutine buildlist_duplicate_trap


!!--------------------------- increment_trap -------------------------------!!
!!--------------------------- increment_trap -------------------------------!!
!!--------------------------- increment_trap -------------------------------!!


      subroutine buildlist_increment_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           ::  maxdo                      !local

      if (parobj%incIncrement .eq. 0) then
        call pc_error ('Increment must be non-zero')
        return
      endif

      maxdo =(parobj%NjobValueList-parobj%incListStart)/abs(parobj%incListInc)+1
      if (parobj%incListTodo.gt.maxdo) then
        parobj%incListTodo = maxdo
      else
        maxdo = parobj%incListTodo
      endif
      call incutils_inc_trap (parobj%incStart,parobj%incEnd,  &
                               parobj%incDuplicate,parobj%incIncrement,  &
                               parobj%incCycles,parobj%incTodo,  &
                               parobj%incFavorEnd,maxdo)

      return
      end subroutine buildlist_increment_trap


!!----------------------------- cycles_trap --------------------------------!!
!!----------------------------- cycles_trap --------------------------------!!
!!----------------------------- cycles_trap --------------------------------!!


      subroutine buildlist_cycles_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           ::  maxdo                      !local

      if (parobj%incCycles .eq. 0) then
        call pc_error ('Cycles must be non-zero')
        return
      endif

      maxdo =(parobj%NjobValueList-parobj%incListStart)/abs(parobj%incListInc)+1
      if (parobj%incListTodo.gt.maxdo) then
        parobj%incListTodo = maxdo
      else
        maxdo = parobj%incListTodo
      endif
      call incutils_cycles_trap (parobj%incStart,parobj%incEnd,  &
                                  parobj%incDuplicate,parobj%incIncrement,  &
                                  parobj%incCycles,parobj%incTodo,  &
                                  parobj%incFavorEnd,maxdo)

      return
      end subroutine buildlist_cycles_trap


!!------------------------------ end_trap ----------------------------------!!
!!------------------------------ end_trap ----------------------------------!!
!!------------------------------ end_trap ----------------------------------!!


      subroutine buildlist_end_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      integer                           ::  maxdo                      !local

      if (parobj%incEnd .eq. CNIL) then
        call pc_error ('End can not be blank')
        return
      endif

      maxdo =(parobj%NjobValueList-parobj%incListStart)/abs(parobj%incListInc)+1
      if (parobj%incListTodo.gt.maxdo) then
        parobj%incListTodo = maxdo
      else
        maxdo = parobj%incListTodo
      endif
      call incutils_end_trap (parobj%incStart,parobj%incEnd,  &
                               parobj%incDuplicate,parobj%incIncrement,  &
                               parobj%incCycles,parobj%incTodo,  &
                               parobj%incFavorEnd,maxdo)

      return
      end subroutine buildlist_end_trap


!!------------------------------ todo_trap ---------------------------------!!
!!------------------------------ todo_trap ---------------------------------!!
!!------------------------------ todo_trap ---------------------------------!!


      subroutine buildlist_todo_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      character(len=PC_LENGTH)          ::  ctemp                      !local
      integer                           ::  maxdo                      !local

      if (parobj%incTodo .le. 0) then
        call pc_error ('Todo must be greater than zero')
        return
      endif

      if (parobj%incTodo .gt. parobj%NjobValueList) then
        call string_ii2cc (parobj%NjobValueList,ctemp)
        call pc_error ('Todo can not be greater than '//trim(ctemp))
        return
      endif

      maxdo =(parobj%NjobValueList-parobj%incListStart)/abs(parobj%incListInc)+1
      if (parobj%incListTodo.gt.maxdo) then
        parobj%incListTodo = maxdo
      else
        maxdo = parobj%incListTodo
      endif
      call incutils_todo_trap (parobj%incStart,parobj%incEnd,  &
                                parobj%incDuplicate,parobj%incIncrement,  &
                                parobj%incCycles,parobj%incTodo,  &
                                parobj%incFavorEnd,maxdo)

      return
      end subroutine buildlist_todo_trap


!!--------------------------- incexecute_trap ------------------------------!!
!!--------------------------- incexecute_trap ------------------------------!!
!!--------------------------- incexecute_trap ------------------------------!!


      subroutine buildlist_incexecute_trap (keyword)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument

      integer                           :: i                           !local
      character(len=PC_LENGTH)          :: list_name                   !local
      character(len=PC_LENGTH)          :: current_name                !local
      type(cfelist_struct),pointer     :: current,next                !local


      if (len_trim(parobj%incListName) .eq. 0) then
        call pc_error ('List name must be specified')
        return
      endif

      list_name = parobj%incListName
!!!   i = index        (list_name,'.lst',.true.)
      i = string_index (list_name,'.lst',.true.)
      if (i .gt. 1) then
        list_name = list_name(1:i-1)
      endif
      call string_to_upper(list_name)

      if (.not. associated(parobj%listBuffer)) then
        call cfelistbuffer_create         (parobj%listBuffer)
        call cfelist_create           (parobj%listDisplayed)
        call cfelist_set_name         (parobj%listDisplayed,list_name)
        call cfelist_get_name         (parobj%listDisplayed,parobj%listName)
        call cfelistbuffer_set_first_list &
                                    (parobj%listBuffer,parobj%listDisplayed)
        call cfelistbuffer_set_last_list  &
                                    (parobj%listBuffer,parobj%listDisplayed)
        if (associated(parobj%listValues)) deallocate(parobj%listValues)
        parobj%NlistValues = parobj%NjobValueList
        allocate(parobj%listValues(parobj%NlistValues))
        parobj%listValues = ''
      else
        call cfelistbuffer_get_last_list(parobj%listBuffer,current)
        if (.not. associated(current)) then
          call cfelist_create          (parobj%listDisplayed)
          call cfelist_set_name        (parobj%listDisplayed,list_name)
          call cfelist_get_name        (parobj%listDisplayed,parobj%listName)
          call cfelistbuffer_set_first_list &
                                    (parobj%listBuffer,parobj%listDisplayed)
          call cfelistbuffer_set_last_list  &
                                    (parobj%listBuffer,parobj%listDisplayed)
          if (associated(parobj%listValues)) deallocate(parobj%listValues)
          parobj%NlistValues = parobj%NjobValueList
          allocate(parobj%listValues(parobj%NlistValues))
          parobj%listValues = ''
        else
          nullify(parobj%listDisplayed)
          call cfelistbuffer_get_first_list (parobj%listBuffer,current)
          do
      if (associated(parobj%listValues_temp)) deallocate(parobj%listValues_temp)
            if (.not. associated(current)) exit
            call cfelist_get_name (current,current_name)
            if (trim(current_name) .eq. trim(list_name)) then
              parobj%listDisplayed => current
              exit
            endif
!!!         call cfelist_get_next (current,current)
            call cfelist_get_next (current,next)
            current => next
          enddo
          if (.not. associated(parobj%listDisplayed)) then
            call cfelist_create         (parobj%listDisplayed)
            call cfelist_set_name       (parobj%listDisplayed,list_name)
            call cfelist_get_name       (parobj%listDisplayed,parobj%listName)
            call cfelistbuffer_insert_list  (parobj%listBuffer,    &
                                          parobj%listDisplayed)
            if (associated(parobj%listValues)) deallocate(parobj%listValues)
            parobj%NlistValues = parobj%NjobValueList
            allocate(parobj%listValues(parobj%NlistValues))
            parobj%listValues = ''
          else
            call cfelist_alloc_strings (parobj%listDisplayed,  &
                                         parobj%listValues,     &
                                         parobj%NlistValues)
            call cfelist_clear         (parobj%listDisplayed)
          endif
        endif
      endif

      if (associated(parobj%listValues_temp)) deallocate(parobj%listValues_temp)
      parobj%NlistValues_temp = parobj%NlistValues
      allocate(parobj%listValues_temp(parobj%NlistValues_temp))
      parobj%listValues_temp =  parobj%listValues

      call incutils_selection ('','*',parobj%incStart,parobj%incDuplicate,  &
                                parobj%incIncrement,parobj%incCycles,  &
                                parobj%incTodo,parobj%listValues,  &
                                parobj%NlistValues,parobj%incListStart,  &
                                parobj%incListInc,parobj%incListTodo)

      call cfelist_put_strings (parobj%listDisplayed,parobj%listValues,  &
                                                      parobj%NlistValues)

      return
      end subroutine buildlist_incexecute_trap


!!---------------------------- inc_undo_trap -------------------------------!!
!!---------------------------- inc_undo_trap -------------------------------!!
!!---------------------------- inc_undo_trap -------------------------------!!


      subroutine buildlist_inc_undo_trap (keyword)
      implicit none
      character(len=*),intent(in)       :: keyword                     !argument

      if (associated(parobj%listValues_temp)) then
        if (parobj%NlistValues.gt.0 .and. parobj%NlistValues_temp.gt.0 .and.  &
            parobj%NlistValues.eq.parobj%NlistValues_temp) then
          parobj%listValues = parobj%listValues_temp
          parobj%NlistValues_temp = 0
          deallocate(parobj%listValues_temp)
          call cfelist_clear       (parobj%listDisplayed)
          call cfelist_put_strings (parobj%listDisplayed,parobj%listValues,  &
                                                          parobj%NlistValues)
        else
          call pc_put_gui (keyword,'Beep',' ')
          call pc_error   ('Nothing to undo.')
          return
        endif
      else
        call pc_put_gui (keyword,'Beep',' ')
        call pc_error   ('Nothing to undo.')
        return
      endif

      return
      end subroutine buildlist_inc_undo_trap


!!---------------------------- job_values_trap -----------------------------!!
!!---------------------------- job_values_trap -----------------------------!!
!!---------------------------- job_values_trap -----------------------------!!


      subroutine buildlist_job_values_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      if (parobj%NjobValueList .lt. parobj%jobTodo) then
        call cfeutil_append_array_element (parobj%jobValueList,       &
                                           parobj%NjobValueList,' ',  &
                                           parobj%jobTodo -           &
                                           parobj%NjobValueList)
      else if (parobj%NjobValueList .gt. parobj%jobTodo) then
        call cfeutil_remove_array_element (parobj%jobValueList,  &
                                           parobj%NjobValueList, &
                                           parobj%jobTodo + 1,   &
                                           parobj%NjobValueList)
      endif

      return
      end subroutine buildlist_job_values_trap


!!--------------------------- list_values_trap -----------------------------!!
!!--------------------------- list_values_trap -----------------------------!!
!!--------------------------- list_values_trap -----------------------------!!


      subroutine buildlist_list_values_trap (keyword)
      implicit none
      character(len=*),intent(in)       ::  keyword                    !argument

      call cfelist_clear       (parobj%listDisplayed)
      call cfelist_put_strings (parobj%listDisplayed,parobj%listValues,  &
                                                      parobj%NlistValues)
      return
      end subroutine buildlist_list_values_trap


!!----------------------------- execute_mlist ------------------------------!!
!!----------------------------- execute_mlist ------------------------------!!
!!----------------------------- execute_mlist ------------------------------!!


      subroutine buildlist_execute_mlist (mask,list,nlist,values,nvalues,  &
                                          start,inc)
      implicit none
      character(len=*),intent(in)       ::  mask                       !argument
      character(len=*),pointer          ::  list(:)                    !argument
      integer         ,intent(in)       ::  nlist                      !argument
      character(len=*),pointer          ::  values(:)                  !argument
      integer         ,intent(in)       ::  nvalues                    !argument
      integer         ,intent(in)       ::  start                      !argument
      integer         ,intent(in)       ::  inc                        !argument

      integer                           ::  i                          !local
      integer                           ::  j                          !local
      integer                           ::  i2                         !local
      integer                           ::  list_indx                  !local
      character(len=PC_LENGTH)          ::  local_mask                 !local
      character(len=PC_LENGTH)          ::  ctemp                      !local


      call incutils_expand_mask (mask,local_mask)

      list_indx = 1
      do i=start,nvalues,inc
        ctemp = values(i)
        i2 = 1
        select case (trim(local_mask))
          case ('^')
            values(i) = trim(list(list_indx)) // trim(ctemp)
          case ('$')
            values(i) = trim(ctemp) // trim(list(list_indx))
          case default
            do j=1,len_trim(local_mask)
              select case (local_mask(j:j))
                case ('#')
                  values(i)(i2:) = ctemp(j:j)
                case ('*')
                 if (list_indx .le. nlist) values(i)(i2:)   = list(list_indx)
                case ('&')
                 if (list_indx .le. nlist) values(i)(i2:i2) = list(list_indx)
                case default
              end select
              i2 = len_trim(values(i)) + 1
            enddo
        end select
        list_indx = list_indx + 1
      enddo

      return
      end subroutine buildlist_execute_mlist


!!----------------------------- execute_slist ------------------------------!!
!!----------------------------- execute_slist ------------------------------!!
!!----------------------------- execute_slist ------------------------------!!


      subroutine buildlist_execute_slist (substr,listbuf,values,nvalues,  &
                                          start, inc)
      implicit none
      character(len=*)       ,intent(in) :: substr                     !argument
      type(cfelistbuffer_struct),pointer :: listbuf                    !argument
      character(len=*)       ,pointer    :: values(:)                  !argument
      integer                ,intent(in) :: nvalues                    !argument
      integer                ,intent(in) :: start                      !argument
      integer                ,intent(in) :: inc                        !argument

      integer                            :: i                          !local
      integer                            :: j                          !local
      integer                            :: i1                         !local
      integer                            :: i2                         !local
      integer                           ::  list_indx                  !local
      integer                            :: nctemp                     !local
      integer                            :: nsubstr                    !local
      integer                            :: nlist                      !local
      integer                            :: ncurrent_name              !local
      character(len=PC_LENGTH)           :: ctemp                      !local
      character(len=PC_LENGTH)           :: current_name               !local
      character(len=PC_LENGTH),pointer   :: list(:)                    !local
      type(cfelist_struct),pointer      :: current,previous           !local
      logical                            :: no_values_changed          !local
      logical                            :: list2short                 !local


      nullify(list)

      nsubstr = len_trim(substr)
      if (nsubstr .eq. 0) then
        call pc_error ('No substitution string specified')
        return
      endif

      do i=start,nvalues,inc
        values(i) = trim(substr)
      enddo

      no_values_changed = .true.
      call cfelistbuffer_get_last_list (listbuf,current)
      do
        if (.not. associated(current)) exit
        call cfelist_get_name      (current,current_name)
        ncurrent_name = len_trim(trim(current_name))
        i1 = index(substr,current_name(1:ncurrent_name))
        if (i1 .gt. 0) then
          call cfelist_alloc_strings (current,list,nlist)
          list2short = .false.
          list_indx  = 1
          do i=start,nvalues,inc
            do
              ctemp  = values(i)
              nctemp = len_trim(ctemp)
              i1   = index(ctemp,'$'//current_name(1:ncurrent_name))
              if (i1 .eq. 0) exit
              i2   = i1 + ncurrent_name + 1
              if (list_indx .le. nlist) then
                values(i)(i1:) = list(list_indx)
                no_values_changed = .false.
              else
                list2short     = .true.
                values(i)(i1:) = ''
              endif
              j = len_trim(values(i))
              if (i2 .le. nctemp) values(i)(j+1:) = ctemp(i2:)
            enddo
            list_indx = list_indx + 1
          enddo
          if (list2short)   &
                        call pc_warning ('List was too short for values needed')
        endif
!!!     call cfelist_get_previous (current,current)
        call cfelist_get_previous (current,previous)
        current => previous
      enddo

      if (no_values_changed) call pc_warning ('No Values changed')

      if (associated(list)) deallocate(list)

      return
      end subroutine buildlist_execute_slist


!!------------------------------- get_name ---------------------------------!!
!!------------------------------- get_name ---------------------------------!!
!!------------------------------- get_name ---------------------------------!!


      subroutine buildlist_get_name (obj,name)
      implicit none
      type(buildlist_struct),pointer       :: obj                      !argument
      character(len=*),intent(out)         :: name                     !argument

      if (associated(obj)) then
        name = obj%name
      else
        name = ' '
      endif

      return
      end subroutine buildlist_get_name


!!-------------------------- get_template_value ----------------------------!!
!!-------------------------- get_template_value ----------------------------!!
!!-------------------------- get_template_value ----------------------------!!


      subroutine buildlist_get_template_value (obj,value)
      implicit none
      type(buildlist_struct),pointer       :: obj                      !argument
      character(len=*),intent(out)         :: value                    !argument

      if (associated(obj)) then
        value = obj%jobvalueTemplate
      else
        value = ' '
      endif

      return
      end subroutine buildlist_get_template_value


!!--------------------------- alloc_valuelist ------------------------------!!
!!--------------------------- alloc_valuelist ------------------------------!!
!!--------------------------- alloc_valuelist ------------------------------!!


      subroutine buildlist_alloc_valuelist (obj,values,nvalues)
      implicit none
      type(buildlist_struct),pointer       :: obj                      !argument
      character(len=*)     ,pointer        :: values(:)                !argument
      integer              ,intent(out)    :: nvalues                  !argument

      if (associated(obj)) then
        if (obj%njobvaluelist .gt. 0) then
          if (associated(values)) deallocate(values)
          nvalues = obj%njobvaluelist
          allocate(values(nvalues))
          values(1:nvalues) = obj%jobvaluelist(1:obj%njobvaluelist)
        else
          nvalues = 0
        endif
      else
        nvalues = 0
      endif

      return
      end subroutine buildlist_alloc_valuelist


!!---------------------------- update_popup --------------------------------!!
!!---------------------------- update_popup --------------------------------!!
!!---------------------------- update_popup --------------------------------!!


      subroutine buildlist_update_popup (obj)
      implicit none
      type(buildlist_struct) ,pointer            :: obj                !argument

      if (.not. associated(obj)) return

      if (associated(obj%listBuffer)) then
        call cfelistbuffer_get_first_list (obj%listBuffer    ,obj%listDisplayed)
        if (associated(obj%listDisplayed)) then
          call cfelist_get_name         (obj%listDisplayed ,obj%listName)
          call cfelist_alloc_strings    (obj%listDisplayed ,obj%listValues, &
                                                             obj%NlistValues)
          call pc_put ('listName'   ,obj%listName   )
          call pc_put ('listValues' ,obj%listValues ,obj%NlistValues)
        else
          obj%listName    = ' '
          obj%NlistValues = 0
          if (associated(obj%listValues)) then
            deallocate(obj%listValues)
            allocate(obj%listValues(1))
          endif
        endif
      else
        if (associated(obj%listDisplayed)) then
          call cfelist_delete (obj%listDisplayed)
        endif
        obj%listName    = ' '
        obj%NlistValues = 0
        if (associated(obj%listValues)) then
          deallocate(obj%listValues)
          allocate(obj%listValues(1))
        endif
      endif

      return
      end subroutine buildlist_update_popup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module buildlist_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

