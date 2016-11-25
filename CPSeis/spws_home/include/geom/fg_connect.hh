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

//------------------------ fg_connect.hh ---------------------//
//------------------------ fg_connect.hh ---------------------//
//------------------------ fg_connect.hh ---------------------//

//             header file for the FgConnect class
//                 not derived from any class
//                    subdirectory geom

     // This class provides all kinds of connections between
     // various parts of the FieldGeometry class.


#ifndef _FG_CONNECT_HH_
#define _FG_CONNECT_HH_


class FgConnect
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:

  class FieldGeometry *_fg;
  class SeisSurvey    *_survey;
  class RpCards       *_rp_cards;
  class PpCards       *_pp_cards;
  class ZtCards       *_zt_cards;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:   // constructor and destructor

  FgConnect (FieldGeometry *fg);
  virtual ~FgConnect();

  void addSeisSurvey (SeisSurvey *survey   ) { _survey    = survey; }
  void addRpCards    (RpCards    *rp_cards ) { _rp_cards  = rp_cards; }
  void addPpCards    (PpCards    *pp_cards ) { _pp_cards  = pp_cards; }
  void addZtCards    (ZtCards    *zt_cards ) { _zt_cards  = zt_cards; }

public:

  FieldGeometry *getFieldGeometry()  const  { return _fg; }
  SeisSurvey    *getSeisSurvey   ()  const  { return _survey; }
  RpCards       *getRpCards      ()  const  { return _rp_cards; }
  PpCards       *getPpCards      ()  const  { return _pp_cards; }
  ZtCards       *getZtCards      ()  const  { return _zt_cards; }
  
  long numChannelsInPattern (long pattern)  const;
  int  getDeadSourceCode    (long line_number, float shotpoint)  const;
  int  getDeadReceiverCode  (long line_number, float shotpoint)  const;
  int  sourceMaybeDead      (long line_number, float shotpoint)  const;
  int  receiverMaybeDead    (long line_number, float shotpoint)  const;

  void receiverPatternsHaveChanged ();
  void zt1CardsHaveChanged ();
  void zt2CardsHaveChanged ();
  void zt3CardsHaveChanged ();
  void zt4CardsHaveChanged ();

  void setDeadSourceCodes  ();
  void setDeadReceiverCodes();

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
