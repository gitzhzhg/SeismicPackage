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

//-------------------------- vfbox_info.cc ---------------------------//
//-------------------------- vfbox_info.cc ---------------------------//
//-------------------------- vfbox_info.cc ---------------------------//

//          implementation file for the VfboxInfo class
//                derived from the SLDatabox class
//                derived from the VfInform class
//                     subdirectory vfgui


#include "vfgui/vfbox_info.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_update.hh"
#include "vf/vf_constants.hh"
#include "vaplots/va_semblance_plot.hh"
#include "vaplots/va_cmp_plot.hh"
#include "sl/sl_prim.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


/****
#define  TESTING
****/


#define  TABLE    VfboxInfo       *table = (VfboxInfo*)data;
#define  DATASET  VfDataset     *dataset = table->manager()->activeDataset();
#define  SPLOT    VaSemblancePlot *splot = table->getVaSemblancePlot();
#define  CPLOT    VaCmpPlot       *cplot = table->getVaCmpPlot();


enum { NSEMB = 1, SACTIVE, SEMXLOC, SEMYLOC, SMATCH,
       NCMP     , CACTIVE, CMPXLOC, CMPYLOC, CMATCH,
       NFUN     ,  ACTIVE,    XLOC,    YLOC, VFID, ERRMSG, RAYMSG, NPICKS };


//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//
//-------------------------- constructor -------------------------------//


VfboxInfo::VfboxInfo(SLDelay *slparent, char *name, VfManager *manager)
           : SLDatabox(slparent, name, NULL, 4),
             VfInform(manager),
                _splot   (NULL),
                _cplot   (NULL)
{
}



//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//
//---------------------------- destructor ------------------------------//

VfboxInfo::~VfboxInfo()
{
}



//----------------- virtual functions overriding VfInform ------------//
//----------------- virtual functions overriding VfInform ------------//
//----------------- virtual functions overriding VfInform ------------//

       // private.

void VfboxInfo::postNewActiveVelocityFunction(VfDataset *dataset)
{
  postTotalChanges(dataset);
}


void VfboxInfo::postTotalChanges(VfDataset *dataset)
{
  if(dataset->notActive()) return;
  long act = dataset->getActiveVelocityFunction();
  if(act == -1) act = dataset->numVelocityFunctions();
  setFocus(VFID, (int)act);
}



//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//
//------------------------- update functions -------------------------//


//////////////////////////// number:


static long nsemb_update(void *data)
{
  TABLE
#ifdef TESTING
  DATASET                                         // testing
  return dataset->numVelocityFunctions() / 2;     // testing
#else
  SPLOT
  if(splot == NULL) return 0;
  return splot->getNumberPanelsInFile();
#endif
}


static long ncmp_update(void *data)
{
  TABLE
  CPLOT
  if(cplot == NULL) return 0;
  return cplot->getNumberPanelsInFile();
}


static long nfun_update(void *data)
{
  TABLE
  DATASET
  return dataset->numVelocityFunctions();
}


static long nsemb_update(void *data, long /*ident*/, long /*index*/)
{
  return nsemb_update(data);
}


static long ncmp_update(void *data, long /*ident*/, long /*index*/)
{
  return ncmp_update(data);
}


static long nfun_update(void *data, long /*ident*/, long /*index*/)
{
  return nfun_update(data);
}



///////////////////////////// active:


static long sactive_update(void *data, long /*ident*/, long index)
{
  TABLE
#ifdef TESTING
  DATASET                                                     // testing
  return (dataset->getActiveVelocityFunction() == index);     // testing
#else
  SPLOT
  if(splot == NULL) return FALSE;
  return (splot->getDisplayedPanelIndex() == index);
#endif
}


static long cactive_update(void *data, long /*ident*/, long index)
{
  TABLE
  CPLOT
  if(cplot == NULL) return FALSE;
  return (cplot->getDisplayedPanelIndex() == index);
}


static long active_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  return (dataset->getActiveVelocityFunction() == index);
}



//////////////////////// coords:


static float semxloc_update(void *data, long /*ident*/, long index)
{
  TABLE
#ifdef TESTING
  if     (index == 4) index = 3;        // testing
  else if(index == 3) index = 2;        // testing
  if(index == 5) return -998.0;         // testing
  DATASET                               // testing
  return dataset->getXloc(index) + 1.5; // testing
#else
  SPLOT
  if(splot == NULL) return FNIL;
  return splot->getPanelXlocation(index);
#endif
}


static float semyloc_update(void *data, long /*ident*/, long index)
{
  TABLE
#ifdef TESTING
  if     (index == 4) index = 3;        // testing
  else if(index == 3) index = 2;        // testing
  if(index == 5) return -998.0;         // testing
  DATASET                               // testing
  return dataset->getYloc(index);       // testing
#else
  SPLOT
  if(splot == NULL) return FNIL;
  return splot->getPanelYlocation(index);
#endif
}



static float cmpxloc_update(void *data, long /*ident*/, long index)
{
  TABLE
  CPLOT
  if(cplot == NULL) return FNIL;
  return cplot->getPanelXlocation(index);
}


static float cmpyloc_update(void *data, long /*ident*/, long index)
{
  TABLE
  CPLOT
  if(cplot == NULL) return FNIL;
  return cplot->getPanelYlocation(index);
}



static float xloc_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  return dataset->getXloc(index);
}


static float yloc_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  return dataset->getYloc(index);
}



/////////////////////////// match:


/*
static long smatch_update(void *data, long ident, long index)
*/
static char *smatch_update(void *data, long ident, long index)
{
  static char  buffer[10];
  static char *blank = "none";
  TABLE
  DATASET
  float xloc = semxloc_update(data, ident, index);
  float yloc = semyloc_update(data, ident, index);
  long ifun = dataset->findMatchingVelfun(xloc, yloc);
/*
  if(ifun == -1) return INIL;
  return ifun+1;
*/
  if(ifun == -1) return blank;
  sprintf(buffer, "%d", ifun+1);
  return buffer;
}


/*
static long cmatch_update(void *data, long ident, long index)
*/
static char *cmatch_update(void *data, long ident, long index)
{
  static char  buffer[10];
  static char *blank = "none";
  TABLE
  DATASET
  float xloc = cmpxloc_update(data, ident, index);
  float yloc = cmpyloc_update(data, ident, index);
  long ifun = dataset->findMatchingVelfun(xloc, yloc);
/*
  if(ifun == -1) return INIL;
  return ifun+1;
*/
  if(ifun == -1) return blank;
  sprintf(buffer, "%d", ifun+1);
  return buffer;
}



/////////// velfuns:


static char *vfid_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  return (char*)dataset->getVfid(index);
}


static char *errmsg_update(void *data, long /*ident*/, long index)
{
  static char buffer[2];
  TABLE
  DATASET
  buffer[0] = (char)dataset->getErrorFlag(index);
  return buffer;
}


static char *raymsg_update(void *data, long /*ident*/, long index)
{
  static char buffer[2];
  TABLE
  DATASET
  buffer[0] = (char)dataset->getRaytraceFlag(index);
  return buffer;
}


static long npicks_update(void *data, long /*ident*/, long index)
{
  TABLE
  DATASET
  return dataset->numPicks(index);
}



//-------------------------- make helper ----------------------------//
//-------------------------- make helper ----------------------------//
//-------------------------- make helper ----------------------------//

void VfboxInfo::makeHelper()
{
  static long p0  =   0; 
  static long m4  =  -4; 
  static long m5  =  -5; 
  static long p5  =   5; 

  int c1 = 1;
  int c2 = 37;
  int c3 = 73;

////////////////// first panel:

     //      ID        PROMPT                     JSW   ISW   ROW  COL  NCHAR
  regIvar3  (NSEMB, "sembl panels     MATCHING"  ,&p0,  &m5,   1,  c1+3, 5);

     //           N            NMAX       ROW COL NCHAR MAXROWS
  regArrays (nsemb_update, nsemb_update,   2, c1,  5,     35);

     //      ID       PROMPT     JSW   ISW   COL NCHAR NDEC
  regIarray (SACTIVE, " "      , &p0,  &m4   ,0,   2);
  regFarray (SEMXLOC, "XCOORD" , &p0,  &p5   ,0,   8,  4);
  regFarray (SEMYLOC, "YCOORD" , &p0,  &p5   ,0,   8,  4);
  regCarray (SMATCH , "VELFUN" , &p0,  &p5   ,0,   5);

  funIvar   (NSEMB  ,         NULL,   nsemb_update);
  funIvar   (SACTIVE,         NULL, sactive_update);
  funFvar   (SEMXLOC,         NULL, semxloc_update);
  funFvar   (SEMYLOC,         NULL, semyloc_update);
  funCvar   (SMATCH ,         NULL,  smatch_update);

////////////////// second panel:

     //      ID       PROMPT                     JSW   ISW   ROW  COL  NCHAR
  regIvar3  (NCMP, "cmp panels       MATCHING"  ,&p0,  &m5,   1,  c2+3, 5);

     //           N            NMAX       ROW COL NCHAR MAXROWS
  regArrays ( ncmp_update,  ncmp_update,   2, c2,  5,     35);

     //      ID       PROMPT     JSW   ISW   COL NCHAR NDEC
  regIarray (CACTIVE, " "      , &p0,  &m4   ,0,   2);
  regFarray (CMPXLOC, "XCOORD" , &p0,  &p5   ,0,   8,  4);
  regFarray (CMPYLOC, "YCOORD" , &p0,  &p5   ,0,   8,  4);
  regCarray (CMATCH , "VELFUN" , &p0,  &p5   ,0,   5);

  funIvar   (NCMP   ,         NULL,    ncmp_update);
  funIvar   (CACTIVE,         NULL, cactive_update);
  funFvar   (CMPXLOC,         NULL, cmpxloc_update);
  funFvar   (CMPYLOC,         NULL, cmpyloc_update);
  funCvar   (CMATCH ,         NULL,  cmatch_update);

////////////////// third panel:

     //      ID       PROMPT            JSW   ISW   ROW  COL  NCHAR
  regIvar3  (NFUN, "velocity functions",&p0,  &m5,   1,  c3+3, 5);

     //           N            NMAX       ROW COL NCHAR MAXROWS
  regArrays ( nfun_update,  nfun_update,   2, c3,  5,     35);

     //      ID      PROMPT     JSW   ISW   COL NCHAR NDEC
  regIarray (ACTIVE, " "      , &p0,  &m4   ,0,   2);
  regCarray (VFID  , "NAME"   , &p0,  &p5   ,0,   8);
  regFarray (XLOC  , "XCOORD" , &p0,  &p5   ,0,   8,  4);
  regFarray (YLOC  , "YCOORD" , &p0,  &p5   ,0,   8,  4);
  regCarray (ERRMSG, "E"      , &p0,  &p5   ,0,   1);
  regCarray (RAYMSG, "R"      , &p0,  &p5   ,0,   1);
  regIarray (NPICKS, "num"    , &p0,  &p5   ,0,   3);

  funIvar   (NFUN  ,         NULL,    nfun_update);
  funIvar   (ACTIVE,         NULL,  active_update);
  funCvar   (VFID  ,         NULL,    vfid_update);
  funFvar   (XLOC  ,         NULL,    xloc_update);
  funFvar   (YLOC  ,         NULL,    yloc_update);
  funCvar   (ERRMSG,         NULL,  errmsg_update);
  funCvar   (RAYMSG,         NULL,  raymsg_update);
  funIvar   (NPICKS,         NULL,  npicks_update);
}



//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//
//------------------------------ end -----------------------------------//
