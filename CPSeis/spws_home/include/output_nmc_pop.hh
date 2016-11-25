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
#include "va.h"
#include "sl/sl_form_pop.hh"
#include "sl/sl_file_pair_plus.hh"
#include "tfio.h"

#define GLBLS ".glbl"
#define BYT   ".byt"

class GlobalRecord 
{
   public:
    int          ndpt;     /* # data points */
    int          nbit;     /* # bit in trace */
    int          ntrc;     /* number of traces */
    int          nbih;     /*number of bits in header */
    int          nhead;    /*number of headers in trace */
    int          curtr;    /* current trace number, always starts at 1 */
    float        dt;       /* sample rate */
    float        tstrt;    /* start time of trace */
    float        trmaxg;   /* max trace value */
    char         *header;  /* string of headers */                   
    char         *name;    /* base file name (w/o extension) */
    char         *token;   /* scratch space for string to float conversion */
    char         *scratch;
    unsigned char *trval;  /* get the trace values  */
    FILE         *in;      /* input file pointer */
};


class OutputNmcPop :  public SLFPopSep {

  private:
       SLFilePairPlus      *_file_pair;
       SLTextBox           *_text_box;
       VelStruct           *_vel;
       struct Cntrl        Cl;
       struct GLBL         G; 
       struct GLBL         Gout;
       GlobalRecord        _glbls;
       float               _tolerance;
       int                 _first_time;
       char                *_input_filename;
       char                *_output_filename;
       char                _errmsg[256];
       float               *_missing_locations;
       char                *_missing_location_string;
       char                *_bytes;
       float               *_headers;
       char                *_char_bytes;
       char                *_char_headers;
       float               *_tasf;
       int                 _num_per_group;

  protected:
       virtual void    DoAction();
       virtual void    UndoInput();

  public:
       OutputNmcPop( Widget            p,
                     char              *name,
                     HelpCtx           hctx,
                     VelStruct         *vel);
       ~OutputNmcPop();
       virtual Widget make(Widget p);
       virtual void manage();
       virtual void reloadDefaults(Boolean do_method= True);
       virtual void reloadSystemDefaults(Boolean do_method);
       static void validateTrap(void *user_data,
                              const char *filename1, const char *filename2,
                              long *valid1, long *valid2,
                              char *info1, char *info2,
                              long *same_datasets);
       int applyNMC();
       GlobalRecord getglbl(int *nhead,int *ntrc,char *name, int *ok);
       int readvel(float times[][NFUNMAX],float velf[][NFUNMAX],
                   long *nvel, long nvelp[], float xlocs[], float ylocs[],
                   int *xhdr, int *yhdr);
       int getHeaders(int trace);
       int getData(int trace);
       int bytfile_writeglbl(GlobalRecord glbl,int ntrc,char velfname[]);
       void cleanUp();
       int reallocateArrays(int num_traces);
};
