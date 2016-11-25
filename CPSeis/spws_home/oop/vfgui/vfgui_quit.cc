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

//-------------------------- vfgui_quit.cc ----------------------------//
//-------------------------- vfgui_quit.cc ----------------------------//
//-------------------------- vfgui_quit.cc ----------------------------//

//          implementation file for the VfguiQuit class
//                  derived from the SLQuit class
//                      subdirectory vfgui


#include "vfgui/vfgui_quit.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sl/sl_app.hh"
#include <stdio.h>



//------------------------ constructor ----------------------------//
//------------------------ constructor ----------------------------//
//------------------------ constructor ----------------------------//


VfguiQuit::VfguiQuit (SLApp *app, SLShellContainer *savepop,
                                              VfManager *manager)
         : SLQuit(app, savepop),
              _manager       (manager)
{
  assert(_manager);
}



//---------------------- destructor -------------------------//
//---------------------- destructor -------------------------//
//---------------------- destructor -------------------------//

VfguiQuit::~VfguiQuit()
{
}



//------------------- virtual data needs saving --------------------//
//------------------- virtual data needs saving --------------------//
//------------------- virtual data needs saving --------------------//


int VfguiQuit::virtualDataNeedsSaving()
{
  long need = _manager->numDatasetsNeedingSaving();
  if(need > 0) return TRUE;
  return FALSE;
}



//----------------- virtual get save info ----------------------------//
//----------------- virtual get save info ----------------------------//
//----------------- virtual get save info ----------------------------//


void VfguiQuit::virtualGetSaveInfo(char *info)
{
  long number = _manager->numDatasets();
  for(int index = 0; index < number; index++)
      {
      if(_manager->dataset(index)->dataNeedsSaving())
          {
          char tidbit[50];
          sprintf(tidbit, "velocity dataset %d needs saving ", index+1);
          strcat(info, tidbit);
          if(_manager->dataset(index)->dataBackedUp())
                 strcat(info, "(backed up)\n");
          else   strcat(info, "(not backed up)\n");
          }
      }
}



//--------------------------- end --------------------------------//
//--------------------------- end --------------------------------//
//--------------------------- end --------------------------------//
