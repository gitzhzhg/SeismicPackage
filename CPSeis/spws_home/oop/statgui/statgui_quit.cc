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

//-------------------------- statgui_quit.cc ----------------------------//
//-------------------------- statgui_quit.cc ----------------------------//
//-------------------------- statgui_quit.cc ----------------------------//

//          implementation file for the StatguiQuit class
//                  derived from the SLDelay class
//                      subdirectory statgui


#include "statgui/statgui_quit.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_app.hh"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>



//------------------------ constructor ----------------------------//
//------------------------ constructor ----------------------------//
//------------------------ constructor ----------------------------//


StatguiQuit::StatguiQuit (SLApp *app, SLShellContainer *savepop,
                                              StaticManager *manager)
         : SLQuit(app, savepop),
              _manager       (manager)
{
  assert(_manager);
}



//---------------------- destructor -------------------------//
//---------------------- destructor -------------------------//
//---------------------- destructor -------------------------//

StatguiQuit::~StatguiQuit()
{
}



//------------------- virtual data needs saving --------------------//
//------------------- virtual data needs saving --------------------//
//------------------- virtual data needs saving --------------------//


int StatguiQuit::virtualDataNeedsSaving()
{
  int need = _manager->numDatasetsNeedingSaving();
  if(need > 0) return TRUE;
  return FALSE;
}



//----------------- virtual get save info ----------------------------//
//----------------- virtual get save info ----------------------------//
//----------------- virtual get save info ----------------------------//


void StatguiQuit::virtualGetSaveInfo(char *info)
{
  int number = _manager->numDatasets();
  for(int index = 0; index < number; index++)
      {
      if(_manager->dataset(index)->dataNeedsSaving())
          {
          char tidbit[50];
          sprintf(tidbit, "static dataset %d needs saving ", index+1);
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
