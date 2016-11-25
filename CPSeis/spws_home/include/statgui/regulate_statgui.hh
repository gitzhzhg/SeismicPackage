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

//---------------------- regulate_statgui.hh ---------------------------//
//---------------------- regulate_statgui.hh ---------------------------//
//---------------------- regulate_statgui.hh ---------------------------//

//             header file for the RegulateStatgui class
//                   not derived from any class
//                       subdirectory statgui


     // this class provides a link between the data object and
     // pushbuttons/togglebuttons which regulate the following actions:
     //        - adding and deleting datasets.
     //        - saving backup files.
     //        - locking and unlocking datasets.
     //        - selecting and unselecting datasets.
     //        - enabling and disabling test inform printouts.

     // this class also regulates the sensitivity of the pushbuttons,
     // and the TRUE/FALSE value of the togglebuttons.

     // this class must be deleted before the referenced pushbuttons
     // and togglebuttons are deleted.

     // recommended labels for the pushbuttons and togglebuttons:
     //
     //  (Push)       backup1     Save Backup File of Active Dataset
     //  (Push)       backup2     Save All Necessary Backup Files
     //  (Push)       compare1    Create New Dataset
     //  (Push)       compare2    Make Duplicate of Active Dataset
     //  (Push)       compare3    Delete Active Dataset
     //  (Tog)        lock1       Lock Changes to Active Dataset
     //  (Push)       lock2       Lock Changes to All Datasets
     //  (Push)       lock3       Unlock Changes to All Datasets
     //  (Tog)        select1     Select Active Dataset
     //  (Push)       select2     Select All Datasets
     //  (Push)       select3     Unselect All Datasets
     //  (Tog)        inform1     Print Test Inform Messages


#ifndef _REGULATE_STATGUI_HH_
#define _REGULATE_STATGUI_HH_

class SLpPush;
class SLpToggle;

class RegulateStatgui
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class StaticManager *_manager;
  class SLDelay       *_parent;  // parent for question popup.
  class StaticInform  *_inform;  // prints messages from all virtual functions.

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

           RegulateStatgui (StaticManager *manager);
  virtual ~RegulateStatgui ();

  void regulateBackup  (SLpPush *backup1, SLpPush *backup2);
  void regulateCompare (SLpPush *compare1, SLpPush *compare2,
                                           SLpPush *compare3);
  void regulateLock    (SLpToggle *lock1, SLpPush *lock2, SLpPush *lock3);
  void regulateSelect  (SLpToggle *select1, SLpPush *select2, SLpPush *select3);
  void regulateInform  (SLpToggle *inform1);

  StaticManager *manager   ()  const  { return _manager; }
  SLDelay       *parent    ()  const  { return _parent; }

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
