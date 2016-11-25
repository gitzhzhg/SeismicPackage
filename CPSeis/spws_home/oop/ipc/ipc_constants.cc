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
#include "ipc/ipc_constants.hh"

struct Application
  {
  int  id;
  char *upper;
  char *lower;
  char *mixed;
  char *description;
};

 //  appl_id    uppercase   lowercase   mixedcase         description

static Application APPLICATIONS[] =
{
 {IPC::CBYT    , "CBYT"    , "cbyt"    , "Cbyt"    , "Seismic Data Viewer"  },
 {IPC::VA      , "VA"      , "va"      , "Va"      , "Velocity Analyzer"    },
 {IPC::CSV     , "CSV"     , "csv"     , "Csv"     , "Cube Slice Viewer"    },
 {IPC::CFG     , "CFG"     , "cfg"     , "CFG"     , "Field Geometry Tool"  },
 {IPC::GEOPRESS, "GEOPRESS", "geopress", "Geopress", "Geopressure predicter"},
 {IPC::CPS     , "CPS"     , "cps"     , "Cps"     , "COP Seismic Processor"},
 {IPC::CFE     , "CFE"     , "cfe"     , "Cfe"     , "CPS Job Builder"      },
 {IPC::JOBMON  , "JOBMON"  , "jobmon"  , "Jobmon"  , "CPS Job Monitor"      },
 {IPC::ANY_APPL, "ANY"     , "any"     , "Any"     , "Any Application"      },
};

static const int NUM_APPLICATIONS = sizeof APPLICATIONS / sizeof(Application);

int IPC::verifyApplication (int id)
{
  int retval = 0;
  for (int k2 = 0; k2 < NUM_APPLICATIONS; k2++) {
    if (APPLICATIONS[k2].id == id) retval = 1;
  }
  return retval;
};

int IPC::applIdFromString (char *appl)
{
  int retval, k2;

  for (k2 = 0, retval = APPL_NOT_SET; appl && retval == APPL_NOT_SET &&
    k2 < NUM_APPLICATIONS; k2++) {
    if (!strcmpCaseInsensitive(appl,APPLICATIONS[k2].upper)) {
      retval = APPLICATIONS[k2].id;
    }
  }
  return retval;
}

char *IPC::stringFromApplId (int id, int which)
{
  assert (id >= APPLICATIONS[0].id && APPLICATIONS[NUM_APPLICATIONS-1].id);

  char *retval;

  switch (which) {
  case LOWER:
    retval = APPLICATIONS[id].lower;
    break;
  case UPPER:
    retval = APPLICATIONS[id].upper;
    break;
  case MIXED:
    retval = APPLICATIONS[id].mixed;
    break;
  case DESCRIPTION:
    retval = APPLICATIONS[id].description;
    break;
  default:
    assert (0);
  }
  return retval;
}

int IPC::strcmpCaseInsensitive (const char *dis, const char *dat)
{
  // are dey even the same lengd?
  int dis_len = strlen (dis);
  int result = dis_len - strlen (dat);

  // compare both as upper case
  int k2, dis_c, dat_c;
  for (k2 = 0; result == 0 && k2 < dis_len; k2++) {
    dis_c = dis[k2];
    dat_c = dat[k2];
    dis_c = toupper (dis_c);
    dat_c = toupper (dat_c);
    result = dis_c - dat_c;
  }
  return result;
}

int IPC::numApplications ()
{
  return NUM_APPLICATIONS;
}

