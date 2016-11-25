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
//************************ COPYRIGHT NOTICE ******************************
//      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        ***
//       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         ***
//************************ COPYRIGHT NOTICE ******************************



//************************************************************************
//***             Class to display mute reference file overlays.       ***
//***             Author:Michael L. Sherrill 01/2002                   ***
//************************************************************************


#ifndef _TP_MUTE_REFERENCE_HH_
#define _TP_MUTE_REFERENCE_HH_

#include "pick/tp_reference_base.hh"
#include "sl/sl_delay.hh"


class TpMuteReference : public TpReferenceBase
{

 public:      // constructor and destructor


  TpMuteReference (SLDelay           *slparent,
                   char              *name,
                   int               io,
                   FileBase          *file,
                   const char        *label,
                   HelpCtx           hctx,
                   class TpPopupBase *pop);
    
  virtual ~TpMuteReference();

public:     // get variables

  float       getFirstDisplayedYbin ()  const  { return _ydisp1; }
  float       getLastDisplayedYbin  ()  const  { return _ydisp2; }
  float       getFirstDisplayedZbin ()  const  { return _zdisp1; }
  float       getLastDisplayedZbin  ()  const  { return _zdisp2; }

  float       getLatestYbinUpdated ()  const;
  float       getLatestZbinUpdated ()  const;

  long        getSwitch()            const;
  long        getInterp()            const;
  long        getNhx   ()            const;
  long        getNhy   ()            const;
  long        getNhz   ()            const;
  float       getXmin  ()            const;
  float       getYmin  ()            const;
  float       getZmin  ()            const;
  float       getXmax  ()            const;
  float       getYmax  ()            const;
  float       getZmax  ()            const;
  float       getYsel  ()            const;
  float       getZsel  ()            const;
  Boolean     allowHeaderInput ()    const  { return _allow_header_input; }

  void  setSwitch           (long  yz_switch);
  void  setInterp           (long  interp);
  void  setNhx              (long  nhx );
  void  setNhy              (long  nhy );
  void  setNhz              (long  nhz );
  void  setYsel             (float ysel, Boolean directional = FALSE);
  void  setZsel             (float zsel, Boolean directional = FALSE);

 

  virtual void doReadCurrentPicks   (float *picks,
                        const float *head, long nwords, long n);

  virtual void doReadPreviousPicks  (float *picks,
                        const float *head, long nwords, long n);

  virtual void doReadNextPicks      (float *picks,
                        const float *head, long nwords, long n);

protected:   // overriding virtual functions  (from SLFileReferencePlus)

  virtual void newFilenameEntered();


private:

  void getValuesFromStaticStructure ();
  void putValuesIntoStaticStructure ();
  Boolean adjustSelectedYbin        (float ysel, Boolean directional);
  Boolean adjustSelectedZbin        (float zsel, Boolean directional);

  struct _MuteStruct *_ss;
  class TpPopupBase  *_pop;
  float _ydisp1;    // first displayed Y coordinate
  float _ydisp2;    // last  displayed Y coordinate
  float _zdisp1;    // first displayed Z coordinate
  float _zdisp2;    // last  displayed Z coordinate

  Boolean _allow_header_input;

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
