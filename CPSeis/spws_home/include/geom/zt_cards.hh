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

//---------------------- zt_cards.hh ---------------------------//
//---------------------- zt_cards.hh ---------------------------//
//---------------------- zt_cards.hh ---------------------------//

//               header file for the ZtCards class
//                  not derived from any class
//                       subdirectory geom 

//    This class contains all ZT cards for field geometry.


#ifndef _ZT_CARDS_HH_
#define _ZT_CARDS_HH_


class ZtCards
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class Zt1Cards  *_zt1_cards;
  class Zt2Cards  *_zt2_cards;
  class Zt3Cards  *_zt3_cards;
  class Zt4Cards  *_zt4_cards;


//---------------- constructor and destructor --------------------//
//---------------- constructor and destructor --------------------//
//---------------- constructor and destructor --------------------//

public:

  ZtCards(class FgInformer *informer, class FgConnect *connect);
  virtual ~ZtCards();


//------------------ get dead trace code -------------------------//
//------------------ get dead trace code -------------------------//
//------------------ get dead trace code -------------------------//

public:   // returns ZT_CODE_NONE if argument error.
          // returns ZT_CODE_ZERO  if trace is killed.
          // returns ZT_CODE_REV   if trace is polarity-reversed.
          // returns ZT_CODE_MISS  if trace is missing on input reels.
          // returns ZT_CODE_NONE  otherwise (trace is live).

  int  getDeadTraceCode    (long       group, long      channel,
                            long source_line, float source_shot,
                            long    rec_line, float    rec_shot)  const;

  int  getDeadSourceCode   (long source_line, float source_shot)  const;
  int  getDeadReceiverCode (long    rec_line, float    rec_shot)  const;
  int  getDeadZt3Code      (long       group, long      channel)  const;
  int  getDeadZt4Code      (long source_line, float source_shot,
                            long    rec_line, float    rec_shot)  const;

  int  getDeadTraceCode    (int dead_source_code, int dead_receiver_code,
                            int use_zt3_cards,    int use_zt4_cards,
                            long       group, long      channel,
                            long source_line, float source_shot,
                            long    rec_line, float    rec_shot)  const;

  int  combineDeadTraceCodes (int code1, int code2)  const;

public:    // return TRUE or FALSE (from ZT4 cards).

  int  sourceMaybeDead     (long source_line, float source_shot)  const;
  int  receiverMaybeDead   (long    rec_line, float    rec_shot)  const;
  int  groupPartlyDead     (long       group)                     const;


//------------------- get and set general ZT values ----------------//
//------------------- get and set general ZT values ----------------//
//------------------- get and set general ZT values ----------------//

public:

  long   numZt1Cards               ()              const;
  long   numZt2Cards               ()              const;
  long   numZt3Cards               ()              const;
  long   numZt4Cards               ()              const;

  long   getActiveZt1CardIndex     ()              const;
  long   getActiveZt2CardIndex     ()              const;
  long   getActiveZt3CardIndex     ()              const;
  long   getActiveZt4CardIndex     ()              const;

  void   setActiveZt1CardIndex     (long ixzt);
  void   setActiveZt2CardIndex     (long ixzt);
  void   setActiveZt3CardIndex     (long ixzt);
  void   setActiveZt4CardIndex     (long ixzt);


//------------------ insert or remove ZT cards --------------------//
//------------------ insert or remove ZT cards --------------------//
//------------------ insert or remove ZT cards --------------------//

public:  // the non-void functions return index )ixzt) where action occurred.
         // the non-void functions return -1 if failed.

  long   appendNewZt1Card           ();
  long   appendNewZt2Card           ();
  long   appendNewZt3Card           ();
  long   appendNewZt4Card           ();

  long   insertNewZt1Card           (long ixzt);
  long   insertNewZt2Card           (long ixzt);
  long   insertNewZt3Card           (long ixzt);
  long   insertNewZt4Card           (long ixzt);

  long   insertNewZt1CardFromBuffer (long ixzt);
  long   insertNewZt2CardFromBuffer (long ixzt);
  long   insertNewZt3CardFromBuffer (long ixzt);
  long   insertNewZt4CardFromBuffer (long ixzt);

  long   deleteZt1Card              (long ixzt);
  long   deleteZt2Card              (long ixzt);
  long   deleteZt3Card              (long ixzt);
  long   deleteZt4Card              (long ixzt);

  long   deleteZt1CardToBuffer      (long ixzt);
  long   deleteZt2CardToBuffer      (long ixzt);
  long   deleteZt3CardToBuffer      (long ixzt);
  long   deleteZt4CardToBuffer      (long ixzt);

  void   deleteAllZt1Cards          ();
  void   deleteAllZt2Cards          ();
  void   deleteAllZt3Cards          ();
  void   deleteAllZt4Cards          ();


//----------------- allocate and free space for ZT cards --------------//
//----------------- allocate and free space for ZT cards --------------//
//----------------- allocate and free space for ZT cards --------------//

public:    //  optional usage.

  void  allocateSpaceForZt1Cards (long nadd);
  void  allocateSpaceForZt2Cards (long nadd);
  void  allocateSpaceForZt3Cards (long nadd);
  void  allocateSpaceForZt4Cards (long nadd);

  void  freeSpaceForZt1Cards     ();
  void  freeSpaceForZt2Cards     ();
  void  freeSpaceForZt3Cards     ();
  void  freeSpaceForZt4Cards     ();


//------------------ get and set ZT card values --------------------//
//------------------ get and set ZT card values --------------------//
//------------------ get and set ZT card values --------------------//

public:      // ZT1 card values

  int    getZt1Code                (long ixzt)  const;
  float  getZt1FromSourceShotpoint (long ixzt)  const;
  float  getZt1ToSourceShotpoint   (long ixzt)  const;
  long   getZt1SourceLineNumber    (long ixzt)  const;

  void   setZt1Code                (long ixzt, int   value);
  void   setZt1FromSourceShotpoint (long ixzt, float value);
  void   setZt1ToSourceShotpoint   (long ixzt, float value);
  void   setZt1SourceLineNumber    (long ixzt, long  value);


public:      // ZT2 card values

  int    getZt2Code                  (long ixzt)  const;
  float  getZt2FromReceiverShotpoint (long ixzt)  const;
  float  getZt2ToReceiverShotpoint   (long ixzt)  const;
  long   getZt2ReceiverLineNumber    (long ixzt)  const;

  void   setZt2Code                  (long ixzt, int   value);
  void   setZt2FromReceiverShotpoint (long ixzt, float value);
  void   setZt2ToReceiverShotpoint   (long ixzt, float value);
  void   setZt2ReceiverLineNumber    (long ixzt, long  value);


public:      // ZT3 card values

  int    getZt3Code                (long ixzt)  const;
  long   getZt3FromGroupNumber     (long ixzt)  const;
  long   getZt3ToGroupNumber       (long ixzt)  const;
  long   getZt3FromTraceNumber     (long ixzt)  const;
  long   getZt3ToTraceNumber       (long ixzt)  const;

  void   setZt3Code                (long ixzt, int   value);
  void   setZt3FromGroupNumber     (long ixzt, long  value);
  void   setZt3ToGroupNumber       (long ixzt, long  value);
  void   setZt3FromTraceNumber     (long ixzt, long  value);
  void   setZt3ToTraceNumber       (long ixzt, long  value);


public:      // ZT4 card values

  int    getZt4Code                  (long ixzt)  const;
  float  getZt4FromSourceShotpoint   (long ixzt)  const;
  float  getZt4ToSourceShotpoint     (long ixzt)  const;
  long   getZt4SourceLineNumber      (long ixzt)  const;
  float  getZt4FromReceiverShotpoint (long ixzt)  const;
  float  getZt4ToReceiverShotpoint   (long ixzt)  const;
  long   getZt4ReceiverLineNumber    (long ixzt)  const;

  void   setZt4Code                  (long ixzt, int   value);
  void   setZt4FromSourceShotpoint   (long ixzt, float value);
  void   setZt4ToSourceShotpoint     (long ixzt, float value);
  void   setZt4SourceLineNumber      (long ixzt, long  value);
  void   setZt4FromReceiverShotpoint (long ixzt, float value);
  void   setZt4ToReceiverShotpoint   (long ixzt, float value);
  void   setZt4ReceiverLineNumber    (long ixzt, long  value);


//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
