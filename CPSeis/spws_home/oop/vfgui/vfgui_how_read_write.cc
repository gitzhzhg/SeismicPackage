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

//------------------------ vfgui_how_read_write.cc ----------------------//
//------------------------ vfgui_how_read_write.cc ----------------------//
//------------------------ vfgui_how_read_write.cc ----------------------//

//        implementation file for the VfguiHowReadWrite class
//                 derived from the SLSmartForm class
//                       subdirectory vfgui


#include "vfgui/vfgui_how_read_write.hh"
#include "vf/vf_diskfile.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
/*
#include "sl/sl_column_number.hh"
*/
#include "sl/slp_push.hh"
#include "sl/slp_label.hh"
#include "sl/slp_option.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include <stream.h>
#include <ctype.h>
#include <assert.h>


#define BIAS                5


//------------------------------- traps ------------------------------------//
//------------------------------- traps ------------------------------------//
//------------------------------- traps ------------------------------------//


static void nhx_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  int nhx = (int)newvar;
  diskfile->setNhx(nhx);
}


static void nhy_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  int nhy = (int)newvar;
  diskfile->setNhy(nhy);
}


static void order_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  int order = (int)newvar;
  diskfile->setMoveoutOrder(order);
}


static void vtype_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  int type = (int)newvar - BIAS;
  diskfile->setType(type);
}


/*
static void units_trap
                (void *data, long / *ident* /, long / *oldvar* /, long newvar)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  diskfile->setDistanceUnits((int)newvar);
}


static void attname_trap
             (void *data, long / *ident* /, char* / *oldvar* /, char *newvar)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  diskfile->setAttributeName(newvar);
}


static void attunits_trap
             (void *data, long / *ident* /, char* / *oldvar* /, char *newvar)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  diskfile->setAttributeUnits(newvar);
}


static void tdunits_trap
             (void *data, long / *ident* /, char* / *oldvar* /, char *newvar)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  diskfile->setTimeDepthUnits(newvar);
}
*/



/*
static void reset_trap (void *data, long / *ident* /)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  diskfile->updatePjarFromKernal();
}
*/



//----------------------------- update functions ---------------------------//
//----------------------------- update functions ---------------------------//
//----------------------------- update functions ---------------------------//


static long nhx_upfun(void *data)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  return (long)diskfile->getNhx();
}


static long nhy_upfun(void *data)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  return (long)diskfile->getNhy();
}


static long order_upfun(void *data)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  return (long)diskfile->getMoveoutOrder();
}


static long vtype_upfun(void *data)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  int type = diskfile->getType();
  return type + BIAS;
}


/*
static long units_upfun(void *data)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  return diskfile->getDistanceUnits();
}


static char *attname_upfun(void *data)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  return (char*)diskfile->getAttributeName();
}


static char *attunits_upfun(void *data)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  return (char*)diskfile->getAttributeUnits();
}


static char *tdunits_upfun(void *data)
{
  VfDiskfile *diskfile = (VfDiskfile*)data;
  return (char*)diskfile->getTimeDepthUnits();
}
*/



//----------------------- sense update functions ---------------------------//
//----------------------- sense update functions ---------------------------//
//----------------------- sense update functions ---------------------------//


static long sense_upfun(void *data)
{                        
  VfDiskfile *diskfile = (VfDiskfile*)data;
  const char *encoding = diskfile->getEncoding();
  return (strcmp(encoding, "ascii") == 0);
}



//------------------------------- strings ------------------------------//
//------------------------------- strings ------------------------------//
//------------------------------- strings ------------------------------//


#define HEADING1A "Characteristics of Validated Velocity File"
#define HEADING1B "Characteristics of Velocity File to Output"
/*
#define HEADING2  "Column Number"
*/



//------------------------------ constructor -----------------------------//
//------------------------------ constructor -----------------------------//
//------------------------------ constructor -----------------------------//


VfguiHowReadWrite::VfguiHowReadWrite (SLDelay *slparent, char *name,
                                      VfDiskfile *diskfile)
            : SLSmartForm(slparent, name)
{
  assert(diskfile);

  SLpLabel *label1 = new SLpLabel (this, "label1" , 0, "NHX:");
  SLpLabel *label2 = new SLpLabel (this, "label2" , 0, "NHY:");
  SLpLabel *label3 = new SLpLabel (this, "label3" , 0, "ORDER:");
  SLpText  *nhx    = new SLpText  (this, "nhx"    , 0, SLPrim::_LONG, 3);
  SLpText  *nhy    = new SLpText  (this, "nhy"    , 0, SLPrim::_LONG, 3);
  SLpText  *order  = new SLpText  (this, "order"  , 0, SLPrim::_LONG, 3);

  int nc = 20;

  if(diskfile->isOutput())
      {
      nhx  ->showLabelAppearance();
      nhy  ->showLabelAppearance();
      order->showLabelAppearance();
      nc = 12;
      }

  SLpLabel  *head1;
  if(diskfile->isInput()) head1 = new SLpLabel (this, "head1", 0, HEADING1A);
  else                    head1 = new SLpLabel (this, "head1", 0, HEADING1B);

/*
  SLpLabel  *head2  = new SLpLabel  (this, "head2", 0, HEADING2);
*/
  SLpOption *vtype  = new SLpOption (this, "vtype", 0, "Velocity Type:");
/*
  SLpOption *units  = new SLpOption (this, "units", 0,
                                               "Depth and Velocity Units:");
*/
/******
  SL2Text *attname  = new SL2Text   (this, "attname" , 0, "Attribute Name:");
  SL2Text *attunits = new SL2Text   (this, "attunits", 0, "Attribute Units:");
  SL2Text *tdunits  = new SL2Text   (this, "tdunits" , 0, "Time Depth Units:");
******/

/*
///////// the above is replaced by the below because the top and bottom
///////// internal attachments in SL2Text do not work if the parent is
///////// put into a container with initial height set to just a few pixels
///////// using default resources.


  SLpLabel *lab1     = new SLpLabel (this, "lab1" , 0, "Attribute Name:");
  SLpLabel *lab2     = new SLpLabel (this, "lab2" , 0, "Attribute Units:");
  SLpLabel *lab3     = new SLpLabel (this, "lab3" , 0, "Time Depth Units:");
  SLpText  *attname  = new SLpText  (this, "attname" );
  SLpText  *attunits = new SLpText  (this, "attunits");
  SLpText  *tdunits  = new SLpText  (this, "tdunits" );
*/

/*
  SLColumnNumber *abscissa = new SLColumnNumber (this, "abscissa",diskfile,nc);
  SLColumnNumber *ordinate = new SLColumnNumber (this, "ordinate",diskfile,nc);
  SLColumnNumber *xcoord   = new SLColumnNumber (this, "xcoord"  ,diskfile,nc);
  SLColumnNumber *ycoord   = new SLColumnNumber (this, "ycoord"  ,diskfile,nc);
  SLColumnNumber *veltype  = new SLColumnNumber (this, "veltype" ,diskfile,nc);
  SLColumnNumber *velname  = new SLColumnNumber (this, "velname" ,diskfile,nc);
*/
/****
  SLColumnNumber *project  = new SLColumnNumber (this, "project" ,diskfile,nc);
  SLColumnNumber *line     = new SLColumnNumber (this, "line"    ,diskfile,nc);
  SLColumnNumber *rdate    = new SLColumnNumber (this, "rdate"   ,diskfile,nc);
  SLColumnNumber *pdate    = new SLColumnNumber (this, "pdate"   ,diskfile,nc);
  SLColumnNumber *userid   = new SLColumnNumber (this, "userid"  ,diskfile,nc);
  SLColumnNumber *comment  = new SLColumnNumber (this, "comment" ,diskfile,nc);
****/

/*
  SLSmartForm *resetrow = NULL;

  if(diskfile->isOutput())
      {
      resetrow       = new SLSmartForm (resetrow, "resetrow");
      SLpPush *reset = new SLpPush (resetrow, "reset", 0, "Reset Defaults");
      reset->setAtrap (reset_trap, diskfile);
      resetrow->showEvenSpacing();
      }
*/


                             //////

  int type1 = FIRSTTYPE;
  int type2 = LASTTYPE;
  char vtype_buff[16] = "vel_file_type_x";
   
  for(int type = type1; type <= type2; type++)
      {
  /*vtype->addOption((char*)VfUtilities::typeDescription(type), type + BIAS);*/
       	vtype_buff[14] = 'a' + (char)type;
	vtype->addOption(vtype_buff, type + BIAS,
		 (char*)VfUtilities::typeDescription(type));
      }
  vtype->addOption("unspecified", -1 + BIAS, "Unspecified");

                             //////

/*
  units->addOption("feet_and_feet/second", FEET_PER_SECOND,
		   "feet and feet/second");    
  units->addOption("meters_and_meters/second", METERS_PER_SECOND,
		   "meters and meters/second");
  units->addOption("unspecified"               , UNSPECIFIED_UNITS,
		   "Unspecified");
*/

                             //////

  nhx     ->setItrap      (      nhx_trap  , diskfile);
  nhy     ->setItrap      (      nhy_trap  , diskfile);
  order   ->setItrap      (    order_trap  , diskfile);
  vtype   ->setItrap      (    vtype_trap  , diskfile);

  nhx     ->setupIvarFun  (      nhx_upfun , diskfile);
  nhy     ->setupIvarFun  (      nhy_upfun , diskfile);
  order   ->setupIvarFun  (    order_upfun , diskfile);
  vtype   ->setupIvarFun  (    vtype_upfun , diskfile);

  head1   ->setupSenseFun (    sense_upfun , diskfile);
  label1  ->setupSenseFun (    sense_upfun , diskfile);
  label2  ->setupSenseFun (    sense_upfun , diskfile);
  label3  ->setupSenseFun (    sense_upfun , diskfile);
  nhx     ->setupSenseFun (    sense_upfun , diskfile);
  nhy     ->setupSenseFun (    sense_upfun , diskfile);
  order   ->setupSenseFun (    sense_upfun , diskfile);
  vtype   ->setupSenseFun (    sense_upfun , diskfile);

/*
  units   ->setItrap      (    units_trap  , diskfile);
  attname ->setCtrap      (  attname_trap  , diskfile);
  attunits->setCtrap      ( attunits_trap  , diskfile);
  tdunits ->setCtrap      (  tdunits_trap  , diskfile);
*/

/*
  units   ->setupIvarFun  (    units_upfun , diskfile);
  attname ->setupCvarFun  (  attname_upfun , diskfile);
  attunits->setupCvarFun  ( attunits_upfun , diskfile);
  tdunits ->setupCvarFun  (  tdunits_upfun , diskfile);
*/

                             //////

     //            LEFT     RIGHT    TOP      BOTTOM
  attach(head1   , this   , this ,  this    ,  NULL,  5,  5,  5);
/*
  attach(head1   , this   , NULL ,  this    ,  NULL,  5,  5,  5);
  attach(head2   , NULL   , this ,  this    ,  NULL,  5,  5,  5);
*/

  attach(label1  , this   , NULL ,  head1   ,  NULL,  5,  5,  5);
  attach(nhx     , label1 , NULL ,  head1   ,  NULL,  5,  5,  5);
  attach(label2  , nhx    , NULL ,  head1   ,  NULL, 25,  5,  5);
  attach(nhy     , label2 , NULL ,  head1   ,  NULL,  5,  5,  5);
  attach(label3  , nhy    , NULL ,  head1   ,  NULL, 25,  5,  5);
  attach(order   , label3 , NULL ,  head1   ,  NULL,  5,  5,  5);

  attach(vtype   , this   , NULL ,  nhx     ,  this,  5,  5,  5,  5);
/*
  attach(units   , this   , NULL ,  vtype   ,  NULL,  5,  5,  0);
*/

/******
  attach(attname , this   , NULL ,  units   ,  NULL,  5,  5,  0);
  attach(attunits, this   , NULL ,  attname ,  NULL,  5,  5,  0);
  attach(tdunits , this   , NULL ,  attunits,  NULL,  5,  5,  0);
******/

/*
///////// the above is replaced by the below for reasons specified above.

  attach(lab1    , this   , NULL ,  units   ,  NULL,  5,  5,  5);
  attach(lab2    , this   , NULL ,  attname ,  NULL,  5,  5,  0);
  attach(lab3    , this   , NULL ,  attunits,  NULL,  5,  5,  0);
  attach(attname , lab1   , NULL ,  units   ,  NULL,  5,  5,  5);
  attach(attunits, lab2   , NULL ,  attname ,  NULL,  5,  5,  0);
  attach(tdunits , lab3   , NULL ,  attunits,  NULL,  5,  5,  0);
*/

/*
  attach(abscissa , NULL   , this , head1     , NULL,  5,  5,  5);
  attach(ordinate , NULL   , this , abscissa  , NULL,  5,  5,  0);
  attach(xcoord   , NULL   , this , ordinate  , NULL,  5,  5,  0);
  attach(ycoord   , NULL   , this , xcoord    , NULL,  5,  5,  0);
  attach(veltype  , NULL   , this , ycoord    , NULL,  5,  5,  0);
  attach(velname  , NULL   , this , veltype   , this,  5,  5,  0,  5);
*/

/*
  if(diskfile->isOutput())
      {
      attach(ordinate , NULL   , this , abscissa  , NULL,  5,  5,  0);
      attach(resetrow , NULL   , this , ordinate  , this,  5,  5,  0,  5);
      }
  else
      {
      attach(ordinate , NULL   , this , abscissa  , this,  5,  5,  0,  5);
      }
*/

/****
  attach(velname  , NULL   , this , veltype   , NULL,  5,  5,  0);
  attach(project  , NULL   , this , velname   , NULL,  5,  5,  0);
  attach(line     , NULL   , this , project   , NULL,  5,  5,  0);
  attach(rdate    , NULL   , this , line      , NULL,  5,  5,  0);
  attach(pdate    , NULL   , this , rdate     , NULL,  5,  5,  0);
  attach(userid   , NULL   , this , pdate     , NULL,  5,  5,  0);
  attach(comment  , NULL   , this , userid    , this,  5,  5,  0,  5);
****/
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfguiHowReadWrite::~VfguiHowReadWrite()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
