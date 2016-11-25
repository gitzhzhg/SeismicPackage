
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
//---------------------- fg_headers.cc ------------------------//
//---------------------- fg_headers.cc ------------------------//
//---------------------- fg_headers.cc ------------------------//

//           implementation file for the FgHeaders class
//                   not derived from any class
//                        subdirectory geom

//    This class calculates and returns CPS trace header words.

//    The array _head[CPS_MAX_HEADERS + 1] is one greater
//    than the number of CPS trace headers so that, for
//    convenience and readability, the index to be used
//    in the array is the CPS header number (1 thru 64)
//    rather than a C-style index (0 thru 63).  This is
//    being done since the CPS header word numbers are
//    generally known.

//    If some trace headers cannot be calculated because of
//    incomplete or invalid information in FieldGeometry,
//    or because of a request for a trace number outside of
//    the valid range, those headers will be set to nil.

//    Note that the trace headers calculated and stored within
//    this object, and returned by this object, are double precision,
//    even though the CPS trace headers are supposed to be floats.


#include "geom/fg_headers.hh"
#include "geom/fg_trace_values.hh"
#include "geom/fg_constants.hh"
#include "oprim/grid_transform.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>


/*
enum { xNONE = -1, xCI = 0, xPS, xPR, xGS, xGR, xGM,
       xTR, xTM, xDT, xAS, xAR, xAM, xFUNCTIONS, xTMASAR, xASAR };

static int xNEED[CPS_MAX_HEADERS] =
     {
  //     1     2     3     4     5     6     7     8     9     10
  xNONE, xDT, xCI, xCI, xNONE, xTM, xTM, xTM, xCI, xCI,      // 1-10
  xGM, xGM, xAS, xTM, xTM, xAR, xTM, xTM, xAM, xAS,          // 11-20
  xNONE, xNONE, xNONE, xAS, xDT, xGS, xTR, xTR, xGS, xAM,    // 21-30
  xTMASAR, xAR, xGM, xGM, xTM, xTM, xAM, xAM, xNONE, xASAR,  // 31-40
  xNONE, xNONE, xNONE, xAS, xNONE, xAS, xAR, xNONE, xNONE, xNONE,           // 41-50
  xNONE, xNONE, xNONE, xNONE, xNONE, xNONE, xNONE, xTM, xTM, xAS,           // 51-60
  xAR, xNONE, xNONE, xDT,           // 61-64
      };

static int xNEED[CPS_MAX_HEADERS][xFUNCTIONS] =
     {
 //    CI  PS  PR    GS  GR  GM    TR  TM  DT    AS  AR  AM
    {   0,  0,  0,    0,  0,  0,    0,  0,  0,    0,  0,  0 },      //  1 INPUT
    { xCI,  0,  0,    0,  0,  0,    0,  0,  0,    0,  0,  0 },      //  2  DT
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      //  3  CI
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      //  4  CI
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      //  5  --
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      //  6  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      //  7  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      //  8  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      //  9  CI
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 10  CI
 //    CI  PS  PR    GS  GR  GM    TR  TM  DT    AS  AR  AM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 11  GM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 12  GM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 13  AS
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 14  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 15  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 16  AR
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 17  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 18  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 19  AM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 20  AS
 //    CI  PS  PR    GS  GR  GM    TR  TM  DT    AS  AR  AM
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 21  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 22  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 23  --
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 24  AS
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 25  DT
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 26  GS
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 27  TR
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 28  TR
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 29  GS
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 30  AM
 //    CI  PS  PR    GS  GR  GM    TR  TM  DT    AS  AR  AM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 31  TM AS AR
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 32  AR
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 33  GM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 34  GM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 35  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 36  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 37  AM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 38  AM
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 39  --
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 40  AS AR
 //    CI  PS  PR    GS  GR  GM    TR  TM  DT    AS  AR  AM
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 41  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 42  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 43  --
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 44  AS
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 45  --
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 46  AS
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 47  AR
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 48  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 49  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 50  --
 //    CI  PS  PR    GS  GR  GM    TR  TM  DT    AS  AR  AM
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 51  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 52  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 53  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 54  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 55  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 56  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 57  --
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 58  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 59  TM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 60  AS
 //    CI  PS  PR    GS  GR  GM    TR  TM  DT    AS  AR  AM
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 61  AR
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 62  --
    {   0,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 63  --
    { xCI,  0,  0,   0, 0, 0,   0, 0, 0,   0, 0, 0 },      // 64  DT
  };
*/



//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//
//-------------------- constructor -------------------------------//


FgHeaders::FgHeaders(FgTraceValues *tv, const GridTransform *transform)
       :
             _tv                      (tv),
             _transform               (transform),
             _zero                    (0.0),
             _one                     (1.0),
             _headers_are_nil         (FALSE)
{
  assert(_tv);
  assert(_transform);
  startHeadersFromScratch();
  assert(_headers_are_nil);
}



//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//
//-------------------- destructor -------------------------------//


FgHeaders::~FgHeaders()
{
}



//------------------- start headers from scratch ----------------------//
//------------------- start headers from scratch ----------------------//
//------------------- start headers from scratch ----------------------//

    // public.

    // to be called before making multiple calls to
    // calculateHeaderWords, or after changes have occurred
    // in the FieldGeometry data object.  this insures that
    // the next call to calculateHeaderWords will do all new
    // calculations, and not just assume that some of the
    // previously-calculated information might still be valid.

void FgHeaders::startHeadersFromScratch()
{
  if(_headers_are_nil) return;
  calculateHeaderWords(0, TRUE);
}



//-------------------- documentation --------------------------//
//-------------------- documentation --------------------------//
//-------------------- documentation --------------------------//

//  SEQUENTIAL GROUND POSITION - This is the LD card number.  If      
//    FIXDIST is positive, this is set differently, as described     
//    below.                                                         

//  INLINE DISTANCE - The distance measured along the (possibly       
//    crooked) line, beginning with a value of zero for the first    
//    flag on that line.  If FIXDIST is not zero, this inline        
//    distance is altered as described below.                        

//  NEAREST CMP quantity - The quantity corresponds to a point on the 
//    (possibly crooked) line closest to the true (X,Y) midpoint,    
//    which may not actually fall on the line.                       

//  CENTER CMP quantity - The quantity corresponds to a point halfway 
//    between the source and receiver as measured along the (possibly
//    crooked) line.  The location of this point is specified by the 
//    average of the source and receiver inline distances.           
                                                                        
//  FIXDIST is the fixed inline distance increment.  If FIXDIST is    
//    not zero, several things happen for each line:                    
//
//  If FIXDIST>0 or FIXDIST<0:
//     (1) The actual inline distance to each flag is replaced by a     
//    regularized measurement, such that the distance from flag to      
//    flag (from one LD card to the next) is assumed equal to the       
//    absolute value of FIXDIST.                                        
//     (2) If there is more than one line, the inline distances to
//    all flags on all lines are set to values that will match up the   
//    shotpoints of all the lines, such that the minimum inline         
//    distance will be zero.  This will give sensible results only    
//    if the shotpoint increment is the same for all lines.             
//
//  If FIXDIST>0:
//     (1) The CMP X-coordinate is replaced by the (now fixed) "center"
//    CMP inline distance, and the CMP Y-coordinate is set to 0. 
//     (2) The "nearest" CMP shotpoint and elevation are replaced by
//    the "center" CMP shotpoint and elevation.             
//     (3) The sequential ground position is set so that matching
//    shotpoints on different lines will have the same value.  This
//    will work correctly only if the LD card shotpoint increment  
//    is the same for all lines, and matches the smallest ground        
//    position spacing for the data set.                                
//     (4) If a source or receiver has an excessively large inline skid,
//    it is moved automatically to a neighboring shotpoint in order to     
//    minimize this skid.  This allows the trace to fall into the       
//    middle of the correct CMP bin when a fixed inline distance is     
//    requested.                                                        



//-------------- get header word description ----------------------//
//-------------- get header word description ----------------------//
//-------------- get header word description ----------------------//

//           LIST OF ALL HEADER WORDS AND HOW THEY ARE SET            
//     [items in square brackets are the values when FIXDIST>0]       
                                                                        
static const char *description[] = {
         " 1 trace sequence number",     
         " 2 head mute",                
         " 3 current group number",    
         " 4 trace number in current group", 
         " 5 fold of stack",                
         " 6 offset",                      
         " 7 CMP X grid",                 
         " 8 CMP Y grid",                
         " 9 original group number",    
         "10 trace number in original group",
         "11 SOURCE X location",            
         "12 SOURCE Y location",           
         "13 SOURCE elevation",           
         "14 RECEIVER X location",       
         "15 RECEIVER Y location",      
         "16 RECEIVER elevation",      
         "17 CMP X location [or inline dist]", 
         "18 CMP Y location [or zero]",       
         "19 nearest [or center] CMP elevation",
         "20 SOURCE   hole depth",             
         "21 RECEIVER hole depth",            
         "22 SOURCE   component number",     
         "23 RECEIVER component number",    
         "24 panel number (field file number)",                
         "25 largest absolute value (LAV)",     
         "26 scratch (SOURCE   line number)",  
         "27 scratch (RECEIVER line number)", 
         "28 scratch (RECEIVER shotpoint)",  
         "29 scratch (SOURCE   shotpoint)", 
         "30 scratch (nearest CMP inline dist)", 
         "31 scratch (center  CMP inline dist)",
         "32 scratch (last trace flag)",       
         "33 SOURCE   X grid",                
         "34 SOURCE   Y grid",               
         "35 RECEIVER X grid",              
         "36 RECEIVER Y grid",             
         "37 nearest [or center] CMP shotpoint",
         "38 CMP line number",                 
         "39  pre-NMO datum shift",           
         "40 post-NMO datum shift",          
         "41 cumulative datum      static", 
         "42 cumulative refraction static",     
         "43 cumulative residual   static",    
         "44 SOURCE   uphole time",           
         "45 RECEIVER uphole time",          
         "46 SOURCE   seq ground position", 
         "47 RECEIVER seq ground position",
         "48 user-defined",               
         "49 user-defined",              
         "50 user-defined",             
         "51 user-defined",            
         "52 user-defined",           
         "53 user-defined",          
         "54 user-defined",         
         "55 user-defined",        
         "56  pre-NMO refraction shift",     
         "57 post-NMO refraction shift",    
         "58 unassigned (CMP X location)", 
         "59 unassigned (CMP Y location)",
         "60 unassigned (SOURCE   inline dist)", 
         "61 unassigned (RECEIVER inline dist)",
         "62 unassigned", 
         "63 unassigned", 
         "64 tail mute",                        
     };


const char *FgHeaders::getHeaderWordDescription(int ihead)  const
{
  static char blank[] = " ";
  if(ihead < 1 || ihead > 64) return blank;
  return description[ihead - 1];
}



//----------------- calculate header words --------------------//
//----------------- calculate header words --------------------//
//----------------- calculate header words --------------------//

    // public.
    // sets all CPS trace header words.
    // must be called BEFORE any calls to getHeaderWordValue or
    //   getHeaderErrorFlag or getHeaderTraceNumber.

    // returns error = FALSE if all of the header is successfully built.
    // returns error = TRUE  if all or part of the header cannot be built.
    // (in this case, some or all headers subsequently returned by
    // getHeaderWord may contain nils rather than the correct
    // information)

    // FgTraceValues remembers requested trace number and error flag
    // and calculated header words for people to fetch later.


int FgHeaders::calculateHeaderWords(long itrace, int more)
{
  int error = _tv->calculateTraceValues(itrace, more);

////////// initialize all headers to nil:

  for(int i = 0; i <= CPS_MAX_HEADERS; i++)
      {
      _head[i] = DNIL;
      }
  if(itrace == 0)
      {
      _headers_are_nil = TRUE;
      return TRUE;
      }
  _headers_are_nil = FALSE;

////////// reset headers associated with dead trace code:

  int  dead = _tv->getDeadTraceCode();
  long ndpt = _tv->getNdpt();
  switch(dead)
    {
    case ZT_CODE_REV : _head[ 2] =      1;
                       _head[64] = ndpt  ; setLong(25, -100); break;
    case ZT_CODE_ZERO: _head[ 2] = ndpt+1;
                       _head[64] = ndpt+1; setLong(25,    0); break;
    case ZT_CODE_LIVE: _head[ 2] =      1;
                       _head[64] = ndpt  ; setLong(25,  100); break;
    case ZT_CODE_MISS: _head[ 2] =      1;
                       _head[64] = ndpt  ; setLong(25, -999); break;
    default: break;
    }

////////// reset headers which are always the same:

  if(_tv->getTraceNumber() != INIL)
      {
      _head[ 5] = _one;        // fold of stack
      _head[21] = _zero;       // receiver depth
      _head[22] = _zero;       // source component number
      _head[23] = _zero;       // receiver component number
      _head[39] = _zero;       // pre-NMO datum shift
      _head[41] = _zero;       // cumulative static
      _head[42] = _zero;       // cumulative static
      _head[43] = _zero;       // cumulative static
      _head[45] = _zero;       // receiver uphole time
      _head[48] = _zero;       // user defined
      _head[49] = _zero;       // user defined
      _head[50] = _zero;       // user defined
      _head[51] = _zero;       // user defined
      _head[52] = _zero;       // user defined
      _head[53] = _zero;       // user defined
      _head[54] = _zero;       // user defined
      _head[55] = _zero;       // user defined
      _head[56] = _zero;       // pre-NMO refr shift
      _head[57] = _zero;       // post-NMO refr shift
      _head[62] = _zero;       // unassigned
      _head[63] = _zero;       // unassigned
      }

////////// reset non-nil values from FgTraceValues:

  setLong  ( 1, _tv->getTraceNumber());
  setLong  ( 3, _tv->getGroupNumber());
  setLong  ( 4, _tv->getChannelNumber());
  setFloat ( 6, _tv->getOffset());
  setLong  ( 9, _tv->getGroupNumber());
  setLong  (10, _tv->getChannelNumber());
  setDouble(11, _tv->getSourceXloc());
  setDouble(12, _tv->getSourceYloc());
  setFloat (13, _tv->getSourceElevation());
  setDouble(14, _tv->getReceiverXloc());
  setDouble(15, _tv->getReceiverYloc());
  setFloat (16, _tv->getReceiverElevation());
  setDouble(17, _tv->getEffectiveCmpXloc());
  setDouble(18, _tv->getEffectiveCmpYloc());
  setFloat (19, _tv->getCmpElevation());
  setFloat (20, _tv->getSourceHoleDepth());
  setLong  (24, _tv->getFieldFileNumber());
  setLong  (26, _tv->getSourceLineNumber());
  setLong  (27, _tv->getReceiverLineNumber());
  setFloat (28, _tv->getReceiverShotpoint());
  setFloat (29, _tv->getSourceShotpoint());
  setDouble(30, _tv->getCmpNearestInlineDistance());
  setDouble(31, _tv->getCmpCenterInlineDistance());
  setLong  (32, _tv->getLastTraceFlag());
  setFloat (37, _tv->getCmpShotpoint());
  setFloat (38, _tv->getCmpLineNumber());
  setFloat (40, _tv->getTotalTraceStatic());
  setFloat (44, _tv->getSourceUpholeTime());
  setLong  (46, _tv->getSourceGroundPosition());
  setLong  (47, _tv->getReceiverGroundPosition());
  setDouble(58, _tv->getCmpXloc());
  setDouble(59, _tv->getCmpYloc());
  setDouble(60, _tv->getSourceInlineFixdist   ());
  setDouble(61, _tv->getReceiverInlineFixdist ());

////////// get grid coordinates:

  if(_head[17] != DNIL && _head[18] != DNIL)
      {
      _head[ 7] = _transform->getXgridCoord(_head[17], _head[18]);
      _head[ 8] = _transform->getYgridCoord(_head[17], _head[18]);
      }
  if(_head[11] != DNIL && _head[12] != DNIL)
      {
      _head[33] = _transform->getXgridCoord(_head[11], _head[12]);
      _head[34] = _transform->getYgridCoord(_head[11], _head[12]);
      }
  if(_head[14] != DNIL && _head[15] != DNIL)
      {
      _head[35] = _transform->getXgridCoord(_head[14], _head[15]);
      _head[36] = _transform->getYgridCoord(_head[14], _head[15]);
      }

  return error;
}



//-------------- get information calculated above ----------------//
//-------------- get information calculated above ----------------//
//-------------- get information calculated above ----------------//

    // public.
    // must be called AFTER calling calculateHeaderWords.

int  FgHeaders::getHeaderErrorFlag   ()            const
{
  return _tv->getErrorFlag();
}


long FgHeaders::getHeaderTraceNumber ()            const
{
  return _tv->getRequestedTraceNumber();
}


long FgHeaders::getHeaderSourceLineIndex ()        const
{
  return _tv->getSourceLineIndex();
}


long FgHeaders::getHeaderSourceFlagIndex ()        const
{
  return _tv->getSourceFlagIndex();
}


long FgHeaders::getHeaderReceiverLineIndex ()      const
{
  return _tv->getReceiverLineIndex();
}


long FgHeaders::getHeaderReceiverFlagIndex ()      const
{
  return _tv->getReceiverFlagIndex();
}



//--------------------- get header word value -----------------//
//--------------------- get header word value -----------------//
//--------------------- get header word value -----------------//

    // public.
    // header word number must be between 1 and CPS_MAX_HEADERS.
    // must be called AFTER calling calculateHeaderWords.

double FgHeaders::getHeaderWordValue(int ihead)  const
{
  assert(ihead >= 1 && ihead <= CPS_MAX_HEADERS);
  return _head[ihead];
}



/*
//--------------------- get header word value -----------------//
//--------------------- get header word value -----------------//
//--------------------- get header word value -----------------//

    // public.
    // header word number must be between 1 and CPS_MAX_HEADERS.
    // need not call calculateHeaderWords first.

double FgHeaders::getHeaderWordValue(long itrace, int ihead)
{
  assert(ihead >= 1 && ihead <= CPS_MAX_HEADERS);
  if(itrace != _itrace)
      {
      for(int i = 0; i < xFUNCTIONS; i++)
          {
          _done[i] = FALSE;
          }
      }
  for(int i = 0; i < xFUNCTIONS; i++)
      {
      if(xNEED[ihead-1][i] && !_done[i])
           {
           switch(i)
                {
                case xCI:  calculateCI(); break;
                case xPS:  calculatePS(); break;
                case xPR:  calculatePR(); break;
                case xGS:  calculateGS(); break;
                case xGR:  calculateGR(); break;
                case xGM:  calculateGM(); break;
                case xTR:  calculateTR(); break;
                case xTM:  calculateTM(); break;
                case xDT:  calculateDT(); break;
                case xAS:  calculateAS(); break;
                case xAR:  calculateAR(); break;
                case xAM:  calculateAM(); break;
                default:  assert(FALSE);
                }
           _done[i] = TRUE;
           }
      }
  return _head[ihead];
}
*/



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
