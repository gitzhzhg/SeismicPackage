/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/

//---------------------- vfgui_regulate.hh ---------------------------//
//---------------------- vfgui_regulate.hh ---------------------------//
//---------------------- vfgui_regulate.hh ---------------------------//

//             header file for the VfguiRegulate class
//                   not derived from any class
//                       subdirectory vfgui


     // this class provides a link between the data object and
     // pushbuttons/togglebuttons which regulate the following actions:
     //        - adding and deleting comparison datasets.
     //        - saving backup files.
     //        - locking and unlocking datasets.
     //        - selecting and unselecting datasets.
     //        - enabling and disabling test inform printouts.

     // this class also regulates the sensitivity of the pushbuttons,
     // and the TRUE/FALSE value of the togglebuttons.

     // this class must be deleted before the referenced pushbuttons
     // and togglebuttons are deleted.

     // in cases where there are two functions with the same name,
     // you must use the function with the:
     //  - SHORTER argument list when there is ONLY ONE editable dataset.
     //  - LONGER argument list if there are TWO OR MORE editable datasets.

     // recommended labels for the pushbuttons (Push) and togglebuttons (Tog),
     // which are needed when the number of editable datasets is one (1),
     // or more than one (2+), or any number(#):
     //
     //  (Push) (2+)  backup1     Save Backup File of Active Dataset
     //  (Push) (2+)  backup2     Save All Necessary Backup Files
     //  (Push) (1)   backup3     Save Backup File
     //  (Push) (#)   compare1    Create New Comparison Dataset
     //  (Push) (#)   compare2    Delete Active Comparison Dataset
     //  (Tog)  (2+)  lock1       Lock Changes to Active Dataset
     //  (Push) (2+)  lock2       Lock Changes to All Editable Datasets
     //  (Push) (2+)  lock3       Unlock Changes to All Editable Datasets
     //  (Tog)  (1)   lock4       Lock Changes to Data
     //  (Tog)  (#)   select1     Select Active Dataset
     //  (Push) (#)   select2     Select All Datasets
     //  (Push) (#)   select3     Unselect All Datasets
     //  (Tog)  (#)   inform1     Print Test Inform Messages


#ifndef _VFGUI_REGULATE_HH_
#define _VFGUI_REGULATE_HH_

class SLpPush;
class SLpToggle;

class VfguiRegulate
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class VfManager *_manager;
  class SLDelay   *_parent;    // parent for question popup.
  class VfInform  *_inform;    // prints messages from all virtual functions.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

           VfguiRegulate (VfManager *manager);
  virtual ~VfguiRegulate ();

  void regulateBackup  (SLpPush *backup1, SLpPush *backup2);
  void regulateBackup  (SLpPush *backup3);
  void regulateCompare (SLpPush *compare1, SLpPush *compare2);
  void regulateLock    (SLpToggle *lock1, SLpPush *lock2, SLpPush *lock3);
  void regulateLock    (SLpToggle *lock4);
  void regulateSelect  (SLpToggle *select1, SLpPush *select2, SLpPush *select3);
  void regulateInform  (SLpToggle *inform1);

  VfManager     *manager   ()  const  { return _manager; }
  SLDelay       *parent    ()  const  { return _parent; }

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
