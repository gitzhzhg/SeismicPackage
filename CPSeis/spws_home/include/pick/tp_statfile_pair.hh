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

//---------------------- tp_statfile_pair.hh -------------------------//
//---------------------- tp_statfile_pair.hh -------------------------//
//---------------------- tp_statfile_pair.hh -------------------------//

//           header file for the TpStatfilePair class
//              derived from the TpPairBase class
//                       subdirectory pick

              // general CPS static file interface 

#ifndef _TP_STATFILE_PAIR_HH_
#define _TP_STATFILE_PAIR_HH_

#include "pick/tp_pair_base.hh"


class TpStatfilePair : public TpPairBase
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

protected:

  class   StaticKernal   *_kernal;
  class   TpPopupBase    *_pop;

private:

  long  _align;          // overlay alignment (XMATCH, XYDIFF).
  long  _xy_switch;      // whether to switch X,Y for overlays (TRUE, FALSE).
  float _xsel;           // selected X bin.
  float _ysel;           // selected Y bin.
  float _xysel;          // selected X or Y bin (either xsel or ysel).
  char  _encoding[22];   // ascii or oldcps or binary.

  Boolean _allow_header_input;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:       // SeisPlot *dummy no longer used.

  TpStatfilePair (SLDelay *slparent, TpPopupBase *pop, class SeisPlot *dummy,
                          const char * const filetype,
                          const char * const extension,
                          const Boolean      required1,
                          const Boolean      required2,
                          const char * const program,
                          const char * const default_stattype);

  virtual ~TpStatfilePair();

  float getFirstDisplayedYbin     ()  const;
  float getLastDisplayedYbin      ()  const;
  float getFirstLatestYbinUpdated ()  const;
  float getLastLatestYbinUpdated  ()  const;

public:      // get variables

  long        getOverlayAlignment ()  const  { return _align; }
  long        getSwitch           ()  const  { return _xy_switch; }
  const char *getStattype         ()  const;
  long        getNhx              ()  const;
  long        getNhy              ()  const;
  long        getNhx2             ()  const;
  long        getNhy2             ()  const;
  float       getX1               ()  const;
  float       getY1               ()  const;
  float       getXinc             ()  const;
  float       getYinc             ()  const;
  long        getNx               ()  const;
  long        getNy               ()  const;
  float       getXend             ()  const;
  float       getYend             ()  const;
  float       getXsel             ()  const  { return _xsel; }
  float       getYsel             ()  const  { return _ysel; }
  float       getXYsel            ()  const  { return _xysel; }
  Boolean     allowHeaderInput    ()  const  { return _allow_header_input; }
  const char *getEncoding         ()  const  { return _encoding; }

public:      // set variables

  void  setOverlayAlignment (long align);
  void  setSwitch           (long xy_switch);
  void  setStattype         (const char *stattype);
  void  setNhx              (long  nhx );
  void  setNhy              (long  nhy );
  void  setNhx2             (long  nhx2);
  void  setNhy2             (long  nhy2);
  void  setX1               (float x1  );
  void  setY1               (float y1  );
  void  setXinc             (float xinc);
  void  setYinc             (float yinc);
  void  setNx               (long  nx  );
  void  setNy               (long  ny  );
  void  setXend             (float xend);
  void  setYend             (float yend);
  void  setXsel             (float xsel);
  void  setYsel             (float ysel);
  void  setXYsel            (float xysel);
  void  setEncoding         (const char *encoding);

protected:   // overriding virtual functions  (from SLFilePairPlus)

  virtual void doValidate (const char *filename1,
                           const char *filename2,
                           long *valid1, long *valid2,
                           char *info1, char *info2,
                           long *same_datasets);

  virtual int doOpen (long status,
                      const char *filename1,
                      const char *filename2,
                      Boolean required1, Boolean required2,
                      FppWorkingMessage *working_message_trap,
                      void              *working_message_data,
                      char *msg);

  virtual void doClose ();

public:      // overriding virtual functions (from TpPairBase)

  virtual void doReadCurrentPicks   (float *picks,
                        const float *head, long nwords, long n);

  virtual void doReadPreviousPicks  (float *picks,
                        const float *head, long nwords, long n);

  virtual void doReadNextPicks      (float *picks,
                        const float *head, long nwords, long n);

  virtual void doReadSelectedPicks  (float *picks,
                        const float *head, long nwords, long n);

  virtual void doSaveCurrentPicks   (float *picks,
                        const float *head, long nwords, long n, long action);

  virtual void doUpdateFile         ();

private:

  Boolean adjustSelectedXbin (float xsel, Boolean directional);
  Boolean adjustSelectedYbin (float ysel, Boolean directional);

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
