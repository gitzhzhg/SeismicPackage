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

//---------------------- statgui_header.cc ------------------------//
//---------------------- statgui_header.cc ------------------------//
//---------------------- statgui_header.cc ------------------------//

//         implementation file for the StatguiHeader class
//               derived from the SLSmartForm class
//                      subdirectory statgui

#include "statgui/statgui_header.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "stat/statio_wrapper.hh"
#include "oprim/history_cards.hh"
#include "sl/sl_file_choice.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/sl_sep.hh"
#include "named_constants.h"
#include "stdio.h"
#include "string.h"


enum  { HEAD, GROUND, HIST, NIL };


//------------------------ get static dataset --------------------//
//------------------------ get static dataset --------------------//
//------------------------ get static dataset --------------------//

     // public.
     // which (above enum) specifies which variable is being accessed.

StaticDataset *StatguiHeader::dataset(int which)  const
{
  if(_display == ACTIVE)
      {
      return _manager->activeDataset();
      }
  if(_display == VALIDATE)
      {
      return NULL;
      }
  if(_display == RESAMPLE)
      {
      if(which ==   HEAD) return _manager->activeDataset();
      if(which == GROUND) return _proposed;
      if(which ==   HIST) return _manager->activeDataset();
      }
  return NULL;   // should not get to here.
}



//-------------------------- static traps -----------------------//
//-------------------------- static traps -----------------------//
//-------------------------- static traps -----------------------//


static void stattype_trap(void *data, long /*ident*/, 
                        char* /*oldvar*/, char* newvar)
{                                                     
  StatguiHeader   *gui     = (StatguiHeader*)data;     
  StaticDataset   *dataset = gui->dataset(HEAD);      
  SLFileChoice    *choice  = gui->choice();          
  StatioWrapper   *statio  = gui->statio();           
  if(dataset)                                   
      {                                        
      dataset->setStattype(newvar);               
      }                                      
  else if(statio)
      {                                    
      statio->setScalarString("stattype",newvar); 
      }                                 
  if (choice) choice->updateFields();  
}


static void nhx_trap(void *data, long /*ident*/,
                        long /*oldvar*/, long newvar)   
{                                                      
  StatguiHeader   *gui     = (StatguiHeader*)data;    
  StaticDataset   *dataset = gui->dataset(HEAD);     
  SLFileChoice    *choice  = gui->choice();         
  StatioWrapper   *statio  = gui->statio();        
  if(dataset)                                     
      {                                          
      dataset->setNhx(newvar);                  
      }                                        
  else if(statio)                             
      {                                      
      statio->setNhx((int)newvar);
      }                                     
  if (choice) choice->updateFields();      
}



static void nhy_trap(void *data, long /*ident*/,
                        long /*oldvar*/, long newvar)   
{                                                      
  StatguiHeader   *gui     = (StatguiHeader*)data;    
  StaticDataset   *dataset = gui->dataset(HEAD);     
  SLFileChoice    *choice  = gui->choice();         
  StatioWrapper   *statio  = gui->statio();        
  if(dataset)                                     
      {                                          
      dataset->setNhy(newvar);                  
      }                                        
  else if(statio)                             
      {                                      
      statio->setNhy((int)newvar);
      }                                     
  if (choice) choice->updateFields();      
}




#define TRAPI(HEAD, nhx_trap, setNhx, keyword)                         \
static void nhx_trap(void *data, long /*ident*/,                       \
                        long /*oldvar*/, long newvar)                  \
{                                                                      \
  StatguiHeader   *gui     = (StatguiHeader*)data;                     \
  StaticDataset   *dataset = gui->dataset(HEAD);                       \
  SLFileChoice    *choice  = gui->choice();                            \
  StatioWrapper   *statio  = gui->statio();                            \
  if(dataset)                                                          \
      {                                                                \
      dataset->setNhx(newvar);                                         \
      }                                                                \
  else if(statio)                                                      \
      {                                                                \
      statio->setScalarInteger(keyword,(int)newvar);                   \
      }                                                                \
  if (choice) choice->updateFields();                                  \
}

/***
TRAPI(HEAD  ,  nhx_trap, setNhx , "nhx")
TRAPI(HEAD  ,  nhy_trap, setNhy , "nhy")
***/
TRAPI(HEAD  , nhx2_trap, setNhx2, "nhx2")
TRAPI(HEAD  , nhy2_trap, setNhy2, "nhy2")
TRAPI(GROUND,   nx_trap, setNx  , "nx")
TRAPI(GROUND,   ny_trap, setNy  , "ny")



#define TRAPF(HEAD, nhx_trap, setNhx, keyword)                         \
static void nhx_trap(void *data, long /*ident*/,                       \
                        float /*oldvar*/, float newvar)                \
{                                                                      \
  StatguiHeader   *gui     = (StatguiHeader*)data;                     \
  StaticDataset   *dataset = gui->dataset(HEAD);                       \
  SLFileChoice    *choice  = gui->choice();                            \
  StatioWrapper   *statio  = gui->statio();                            \
  if(dataset)                                                          \
      {                                                                \
      dataset->setNhx(newvar);                                         \
      }                                                                \
  else if(statio)                                                      \
      {                                                                \
      statio->setScalarFloat(keyword,newvar);                          \
      }                                                                \
  if (choice) choice->updateFields();                                  \
}

TRAPF(GROUND,   x1_trap, setX1  , "x1")
TRAPF(GROUND,   y1_trap, setY1  , "y1")
TRAPF(GROUND, xinc_trap, setXinc, "xinc")
TRAPF(GROUND, yinc_trap, setYinc, "yinc")


static void xend_trap(void *data, long /*ident*/,
                        float /*oldvar*/, float newvar) 
{                                                      
  StatguiHeader   *gui     = (StatguiHeader*)data;      
  StaticDataset   *dataset = gui->dataset(HEAD);       
  SLFileChoice    *choice  = gui->choice();           
  StatioWrapper   *statio  = gui->statio();           
  if(dataset)                                    
      {                                         
      dataset->setXend(newvar);                 
      }                                       
  else if(statio)
      {                                     
      float x1   = statio->getScalarFloat("x1");
      float xinc = statio->getScalarFloat("xinc");
      if (xinc > 0.0 && xinc != FNIL && x1 != FNIL)
           {
           float xx = (newvar - x1) / xinc + 1.0;
           int   nx = NearestInteger(xx);
           if (nx >= 1) statio->setScalarInteger("nx",nx);    
           }
      }                                  
  if (choice) choice->updateFields();   
}



static void yend_trap(void *data, long /*ident*/,
                        float /*oldvar*/, float newvar) 
{                                                      
  StatguiHeader   *gui     = (StatguiHeader*)data;      
  StaticDataset   *dataset = gui->dataset(HEAD);       
  SLFileChoice    *choice  = gui->choice();           
  StatioWrapper   *statio  = gui->statio();           
  if(dataset)                                    
      {                                         
      dataset->setYend(newvar);                 
      }                                       
  else if(statio)
      {                                     
      float y1   = statio->getScalarFloat("y1");
      float yinc = statio->getScalarFloat("yinc");
      if (yinc > 0.0 && yinc != FNIL && y1 != FNIL)
           {
           float yy = (newvar - y1) / yinc + 1.0;
           int   ny = NearestInteger(yy);
           if (ny >= 1) statio->setScalarInteger("ny",ny);    
           }
      }                                  
  if (choice) choice->updateFields();   
}



//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//


static char *stattype_update(void *data)
{                                  
  static char blank[] = "";
  StatguiHeader   *gui     = (StatguiHeader*)data;
  StaticDataset   *dataset = gui->dataset(HEAD);
  StatioWrapper   *statio  = gui->statio();           
  if(dataset)                         
      {                                
      return (char*)dataset->getStattype(); 
      }                                  
  else if(statio)
      {                                    
      return (char*)statio->getScalarString("stattype");  
      }              
  return blank;
}


#define UPDATEI(HEAD, nhx_update, getNhx, keyword)                     \
static long nhx_update(void *data)                                     \
{                                                                      \
  StatguiHeader   *gui     = (StatguiHeader*)data;                     \
  StaticDataset   *dataset = gui->dataset(HEAD);                       \
  StatioWrapper   *statio  = gui->statio();                            \
  if(dataset)                                                          \
      {                                                                \
      return dataset->getNhx();                                        \
      }                                                                \
  else if(statio)                                                      \
      {                                                                \
      return (long)statio->getScalarInteger(keyword);                  \
      }                                                                \
  return (long)INIL;                                                   \
}

UPDATEI(HEAD  , nhx2_update, getNhx2         ,   "nhx2")
UPDATEI(HEAD  , nhy2_update, getNhy2         ,   "nhy2")
UPDATEI(HEAD  ,  nhx_update, getNhx          ,   "nhx")
UPDATEI(HEAD  ,  nhy_update, getNhy          ,   "nhy")
UPDATEI(GROUND,   nx_update, getNx           ,   "nx")
UPDATEI(GROUND,   ny_update, getNy           ,   "ny")
UPDATEI(NIL   ,  nil_update, numNilValues    ,   "numnils")



static long card_update(void *data)               
{                                                  
  StatguiHeader   *gui     = (StatguiHeader*)data;    
  StaticDataset   *dataset = gui->dataset(HEAD);       
  StatioWrapper   *statio  = gui->statio();           
  if(dataset)
      {     
      return dataset->history()->numHistoryCards();     
      }      
  else if(statio)                     
      {       
      return (long)statio->numHistoryCards(); 
      }        
  return INIL;
}



#define UPDATEF(HEAD, nhx_update, getNhx, keyword)                     \
static float nhx_update(void *data)                                    \
{                                                                      \
  StatguiHeader   *gui     = (StatguiHeader*)data;                     \
  StaticDataset   *dataset = gui->dataset(HEAD);                       \
  StatioWrapper   *statio  = gui->statio();                            \
  if(dataset)                                                          \
      {                                                                \
      return dataset->getNhx();                                        \
      }                                                                \
  else if(statio)                                                      \
      {                                                                \
      return statio->getScalarFloat(keyword);                          \
      }                                                                \
  return FNIL;                                                         \
}

UPDATEF(GROUND,   x1_update, getX1       ,   "x1")
UPDATEF(GROUND,   y1_update, getY1       ,   "y1")
UPDATEF(GROUND, xinc_update, getXinc     ,   "xinc")
UPDATEF(GROUND, yinc_update, getYinc     ,   "yinc")
UPDATEF(GROUND, vmin_update, minimumValue,   "statmin")
UPDATEF(GROUND, vmax_update, maximumValue,   "statmax")



static float xend_update(void *data)
{
  StatguiHeader   *gui     = (StatguiHeader*)data;
  StaticDataset   *dataset = gui->dataset(GROUND);
  StatioWrapper   *statio  = gui->statio();           
  if(dataset)                 
      {                        
      return dataset->getXend();
      }                      
  else if(statio)
      {                                     
      float x1   = statio->getScalarFloat  ("x1");    
      float xinc = statio->getScalarFloat  ("xinc");    
      int   nx   = statio->getScalarInteger("nx");    
      if(nx == INIL || x1 == FNIL || xinc == FNIL) return FNIL;
      return (x1 + (nx - 1) * xinc);                     
      }                                
  return FNIL;   
}



static float yend_update(void *data)
{
  StatguiHeader   *gui     = (StatguiHeader*)data;
  StaticDataset   *dataset = gui->dataset(GROUND);
  StatioWrapper   *statio  = gui->statio();           
  if(dataset)                 
      {                        
      return dataset->getYend();
      }                      
  else if(statio)
      {                                 
      float y1   = statio->getScalarFloat  ("y1");    
      float yinc = statio->getScalarFloat  ("yinc");    
      int   ny   = statio->getScalarInteger("ny");    
      if(ny == INIL || y1 == FNIL || yinc == FNIL) return FNIL;
      return (y1 + (ny - 1) * yinc);                     
      }                            
  return FNIL;    
}



//----------------- static sense update functions ------------------//
//----------------- static sense update functions ------------------//
//----------------- static sense update functions ------------------//

static long head_sense_update(void *data)
{                                
  StatguiHeader   *gui     = (StatguiHeader*)data;
  StaticDataset   *dataset = gui->dataset(HEAD);
  StatioWrapper   *statio  = gui->statio();           
  if(statio) return TRUE;
  return !dataset->isLocked(); 
}


static long ground_sense_update(void *data)
{                                
  StatguiHeader   *gui     = (StatguiHeader*)data;
  StaticDataset   *dataset = gui->dataset(GROUND);
  StatioWrapper   *statio  = gui->statio();           
  if(statio) return TRUE;
  return (dataset->allowChangingFileSize() && !dataset->isLocked()); 
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//

static char *NAME2 = "statgui_header_large_print";
static char *NAME1 = "statgui_header";


static const char *defres2[] = {
        ".borderWidth:        2",
        "*background:         gray80",
        ".sep.separatorType:  SINGLE_LINE",
        ".sep.height:         6",
        "*fontList:           8x13bold",
            NULL };


static const char *defres1[] = {
        ".borderWidth:        2",
        "*background:         gray80",
        ".sep.separatorType:  SINGLE_LINE",
        ".sep.height:         6",
        "*fontList:           fixed",
            NULL };



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


StatguiHeader::StatguiHeader(SLDelay *slparent,
                     StaticDataset *proposed, StaticManager *manager,
                     const char *title, int editable, int display,
                     SLFileChoice *choice, StatioWrapper *statio)
  : SLSmartForm(slparent, (editable == EDITABLE_LARGE_PRINT ? NAME2 : NAME1)),
               _proposed   (proposed),
               _manager    (manager),
               _display    (display),
               _choice     (choice),
               _statio     (statio)
{
  assert(slparent);
  assert(_proposed || _manager || _statio);
  assert(editable == NOT_EDITABLE ||
         editable == EDITABLE_SMALL_PRINT ||
         editable == EDITABLE_LARGE_PRINT);

  if(editable == EDITABLE_LARGE_PRINT) setFallbackResources(defres2);
  else                                 setFallbackResources(defres1);

/////////////// create widgets:

  SLpLabel *label  = new SLpLabel (this, (char*)title);
  SLSep    *sep    = new SLSep    (this, "sep");
  SLpLabel *label0 = new SLpLabel (this, "type:");
  SLpLabel *label1 = new SLpLabel (this, "header words...");
  SLpLabel *label2 = new SLpLabel (this, "(SER headers)..");
  SLpLabel *label3 = new SLpLabel (this, "first bin......");
  SLpLabel *label4 = new SLpLabel (this, "bin increment..");
  SLpLabel *label5 = new SLpLabel (this, "last bin.......");
  SLpLabel *label6 = new SLpLabel (this, "# bins.........");
  SLpLabel *label7 = new SLpLabel (this, "# history cards....");
  SLpLabel *label8 = new SLpLabel (this, "# nil values.......");
  SLpLabel *label9 = new SLpLabel (this, "min/max values.....");

                     new SLpLabel (this, " ");
                     new SLpLabel (this, " ");

  SLpText  *t0  = new SLpText  (this, "text0", 0, SLpText::_CHAR, 8);
  SLpLabel *xy  = new SLpLabel (this, " X bin     Y bin");

  SLpText  *t1a = new SLpText (this, "t1a", 0, SLpText::_LONG , 8);
  SLpText  *t1b = new SLpText (this, "t1b", 0, SLpText::_LONG , 8);
  SLpText  *t2a = new SLpText (this, "t2a", 0, SLpText::_LONG , 8);
  SLpText  *t2b = new SLpText (this, "t2b", 0, SLpText::_LONG , 8);
  SLpText  *t3a = new SLpText (this, "t3a", 0, SLpText::_FLOAT, 8, 6);
  SLpText  *t3b = new SLpText (this, "t3b", 0, SLpText::_FLOAT, 8, 6);
  SLpText  *t4a = new SLpText (this, "t4a", 0, SLpText::_FLOAT, 8, 6);
  SLpText  *t4b = new SLpText (this, "t4b", 0, SLpText::_FLOAT, 8, 6);
  SLpText  *t5a = new SLpText (this, "t6a", 0, SLpText::_FLOAT, 8, 6);
  SLpText  *t5b = new SLpText (this, "t6b", 0, SLpText::_FLOAT, 8, 6);
  SLpText  *t6a = new SLpText (this, "t5a", 0, SLpText::_LONG , 8);
  SLpText  *t6b = new SLpText (this, "t5b", 0, SLpText::_LONG , 8);
  SLpText  *t7  = new SLpText (this, "t7" , 0, SLpText::_LONG , 8, 0);
  SLpText  *t8  = new SLpText (this, "t8" , 0, SLpText::_LONG , 8, 0);
  SLpText  *t9a = new SLpText (this, "t9a", 0, SLpText::_FLOAT, 6, 1);
  SLpText  *t9b = new SLpText (this, "t9b", 0, SLpText::_FLOAT, 6, 1);

/////////////// register traps:

  t0 ->setCtrap      (stattype_trap, this);
  t1a->setItrap      (     nhx_trap, this);
  t1b->setItrap      (     nhy_trap, this);
  t2a->setItrap      (    nhx2_trap, this);
  t2b->setItrap      (    nhy2_trap, this);
  t3a->setFtrap      (      x1_trap, this);
  t3b->setFtrap      (      y1_trap, this);
  t4a->setFtrap      (    xinc_trap, this);
  t4b->setFtrap      (    yinc_trap, this);
  t5a->setFtrap      (    xend_trap, this);
  t5b->setFtrap      (    yend_trap, this);
  t6a->setItrap      (      nx_trap, this);
  t6b->setItrap      (      ny_trap, this);

/////////////// register update functions:

  t0 ->setupCvarFun  (stattype_update, this);
  t1a->setupIvarFun  (     nhx_update, this);
  t1b->setupIvarFun  (     nhy_update, this);
  t2a->setupIvarFun  (    nhx2_update, this);
  t2b->setupIvarFun  (    nhy2_update, this);
  t3a->setupFvarFun  (      x1_update, this);
  t3b->setupFvarFun  (      y1_update, this);
  t4a->setupFvarFun  (    xinc_update, this);
  t4b->setupFvarFun  (    yinc_update, this);
  t5a->setupFvarFun  (    xend_update, this);
  t5b->setupFvarFun  (    yend_update, this);
  t6a->setupIvarFun  (      nx_update, this);
  t6b->setupIvarFun  (      ny_update, this);
  t7 ->setupIvarFun  (    card_update, this);
  t8 ->setupIvarFun  (     nil_update, this);
  t9a->setupFvarFun  (    vmin_update, this);
  t9b->setupFvarFun  (    vmax_update, this);

/////////////// determine sensitivity and appearance:

  int edit_HEAD   = (editable != NOT_EDITABLE && _display != RESAMPLE);
  int edit_GROUND = (editable != NOT_EDITABLE);

  if(edit_HEAD)
      {
      t0 ->setupSenseFun (head_sense_update, this);
      t1a->setupSenseFun (head_sense_update, this);
      t1b->setupSenseFun (head_sense_update, this);
      t2a->setupSenseFun (head_sense_update, this);
      t2b->setupSenseFun (head_sense_update, this);
      }
  else
      {
      t0 ->showLabelAppearance();
      t1a->showLabelAppearance();
      t1b->showLabelAppearance();
      t2a->showLabelAppearance();
      t2b->showLabelAppearance();
      }

  if(edit_GROUND)
      {
      t3a->setupSenseFun (ground_sense_update, this);
      t3b->setupSenseFun (ground_sense_update, this);
      t4a->setupSenseFun (ground_sense_update, this);
      t4b->setupSenseFun (ground_sense_update, this);
      t5a->setupSenseFun (ground_sense_update, this);
      t5b->setupSenseFun (ground_sense_update, this);
      t6a->setupSenseFun (ground_sense_update, this);
      t6b->setupSenseFun (ground_sense_update, this);
      }
  else
      {
      t3a->showLabelAppearance();
      t3b->showLabelAppearance();
      t4a->showLabelAppearance();
      t4b->showLabelAppearance();
      t5a->showLabelAppearance();
      t5b->showLabelAppearance();
      t6a->showLabelAppearance();
      t6b->showLabelAppearance();
      }

  t7 ->showLabelAppearance();
  t8 ->showLabelAppearance();
  t9a->showLabelAppearance();
  t9b->showLabelAppearance();

/////////////// attachments:

      //           left     right   top   bottom

  attach(label   , this   , this,   this, NULL    , 0, 0, 5, 0);
  attach(sep     , this   , this,  label, NULL    , 0, 0, 0, 0);

  attach(label0  , this   , NULL,   sep , NULL    , 5, 0, 0, 0);
  attach(label1  , this   , NULL,   t0  , NULL    , 5, 0, 0, 0);
  attach(label2  , this   , NULL,   t1a , NULL    , 5, 0, 0, 0);
  attach(label3  , this   , NULL,   t2a , NULL    , 5, 0, 0, 0);
  attach(label4  , this   , NULL,   t3a , NULL    , 5, 0, 0, 0);
  attach(label5  , this   , NULL,   t4a , NULL    , 5, 0, 0, 0);
  attach(label6  , this   , NULL,   t5a , NULL    , 5, 0, 0, 0);
  attach(label7  , this   , NULL,   t6a , NULL    , 5, 0, 0, 0);
  attach(label8  , this   , NULL,   t7  , NULL    , 5, 0, 0, 0);
  attach(label9  , this   , NULL,   t8  , NULL    , 5, 0, 0, 0);

  attach(t0      , label0 , NULL,   sep , NULL    , 0, 0, 0, 0);
  attach(t1a     , label1 , NULL,   t0  , NULL    , 0, 0, 0, 0);
  attach(t2a     , label1 , NULL,   t1a , NULL    , 0, 0, 0, 0);
  attach(t3a     , label1 , NULL,   t2a , NULL    , 0, 0, 0, 0);
  attach(t4a     , label1 , NULL,   t3a , NULL    , 0, 0, 0, 0);
  attach(t5a     , label1 , NULL,   t4a , NULL    , 0, 0, 0, 0);
  attach(t6a     , label1 , NULL,   t5a , NULL    , 0, 0, 0, 0);
  attach(t7      , label7 , NULL,   t6a , NULL    , 0, 0, 0, 0);
  attach(t8      , label8 , NULL,   t7  , NULL    , 0, 0, 0, 0);
  attach(t9a     , label9 , NULL,   t8  , NULL    , 0, 0, 0, 0);

  attach(xy      , t0     , NULL,   sep , NULL    , 0, 0, 0, 0);
  attach(t1b     , t1a    , NULL,   t0  , NULL    , 0, 0, 0, 0);
  attach(t2b     , t1a    , NULL,   t1a , NULL    , 0, 0, 0, 0);
  attach(t3b     , t1a    , NULL,   t2a , NULL    , 0, 0, 0, 0);
  attach(t4b     , t1a    , NULL,   t3a , NULL    , 0, 0, 0, 0);
  attach(t5b     , t1a    , NULL,   t4a , NULL    , 0, 0, 0, 0);
  attach(t6b     , t1a    , NULL,   t5a , NULL    , 0, 0, 0, 0);
  attach(t9b     , t9a    , NULL,   t8  , NULL    , 5, 0, 0, 0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


StatguiHeader::~StatguiHeader()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
