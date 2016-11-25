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

//------------------------ jd_file.hh ----------------------------//
//------------------------ jd_file.hh ----------------------------//
//------------------------ jd_file.hh ----------------------------//

//             header file for the JdFile class
//             derived from the FileBase class
//             derived from the FgInform class
//                    subdirectory geom


//    This class reads and writes CPS JD files.
//    This class accesses the FieldGeometry class.


#ifndef _JD_FILE_HH_
#define _JD_FILE_HH_

#include "oprim/file_base.hh"
#include "geom/fg_inform.hh"


class JdFile  :  public FileBase, public FgInform
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

     // also protected _fg in FgInform base class.
     // also int io = ioIndex() from FileBase base class.

private:  // _geomio[0] = input validate and parameters.
          // _geomio[1] = output validate (overwrite) only.
          // _geomio[2] = output parameters only.

  class GeomioWrapper *_geomio[3];

private:  // directive on which parts of JD file to read.
          // these flags are used when readJdFile is called.
          // these are set to TRUE or FALSE.

  int      _ld_read_flag;
  int      _rp_read_flag;
  int      _pp_read_flag;
  int     _zt1_read_flag;
  int     _zt2_read_flag;
  int     _zt3_read_flag;
  int     _zt4_read_flag;
  int    _grid_read_flag;

private:  // whether to clear data before reading (irrelevant for saving).
          // these flags are used when readJdFile is called.
          // these are set to TRUE or FALSE.

  int    _ld_clear_flag;
  int    _rp_clear_flag;
  int    _pp_clear_flag;
  int   _zt1_clear_flag;
  int   _zt2_clear_flag;
  int   _zt3_clear_flag;
  int   _zt4_clear_flag;

private:  // directive on which parts of data to save onto a new JD file.
          // these flags are used when saveJdFile is called.
          // these are set to TRUE or FALSE.

  int      _ld_save_flag;
  int      _rp_save_flag;
  int      _pp_save_flag;
  int     _zt1_save_flag;
  int     _zt2_save_flag;
  int     _zt3_save_flag;
  int     _zt4_save_flag;
  int    _grid_save_flag;

private:  // if FALSE, reads data like usual.
          // if TRUE, only replaces matching data, and ignores everything else.

  int    _repsel;     // replace selected matching LD cards if TRUE.
  int    _repany;     // replace   any    matching LD cards if TRUE.

//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//
//---------------------- functions ------------------------------//

public:

  JdFile (class FieldGeometry *fg, Intent intent);
  virtual ~JdFile();

  class GeomioWrapper *geomioInput           ()  const  { return _geomio[0]; }
  class GeomioWrapper *geomioOutputOverwrite ()  const  { return _geomio[1]; }
  class GeomioWrapper *geomioOutputParameters()  const  { return _geomio[2]; }

public:  // call this when something in the field geometry data object
         //  changes which might affect the pickle jar parameters, or
         //  when saving a static file.

  void updatePjar ();

protected:   // overriding virtual functions.

  virtual Prepare  virtualPrepareRead (const char *filename, char *errmsg);
//virtual Prepare  virtualPrepareSave (const char *filename, char *errmsg);
  virtual Result   virtualRead        (const char *filename, char *errmsg);
  virtual Result   virtualSave        (const char *filename, char *errmsg);
  virtual Validity virtualValidate    (const char *filename, char *info);
  virtual void     preNewFilename     ();
  virtual void     postNewFilename    ();

private:   // private helper functions.

  static int checkAppendStatus (long index, const char *type, long i, long n,
                                              char *errmsg);

  static int getChaining (const char *buffer, char *errmsg);
  static int getRpFlag   (const char *buffer,                   long i, long n,
                                              char *errmsg);
  static int getZtCode   (const char *buffer, const char *type, long i, long n,
                                              char *errmsg);

  static const char *getChainingString (int chaining);
  static const char *getRpFlagString   (int rpflag);
  static const char *getZtCodeString   (int ztcode);

private:   // main private functions.

  int  readJdHeader     (           char *errmsg);
  int  readLdCards      (long nld , char *errmsg);
  int  readRpCards      (long nrp , char *errmsg);
  int  readPpCards      (long npp , char *errmsg);
  int  readZt1Cards     (long nzt1, char *errmsg);
  int  readZt2Cards     (long nzt2, char *errmsg);
  int  readZt3Cards     (long nzt3, char *errmsg);
  int  readZt4Cards     (long nzt4, char *errmsg);

  int  saveLdCards      ();
  int  saveRpCards      ();
  int  savePpCards      ();
  int  saveZt1Cards     ();
  int  saveZt2Cards     ();
  int  saveZt3Cards     ();
  int  saveZt4Cards     ();

  void skipRead         ();

public:   // get values.
          // grid transform status is 1(present) or -1(absent).
          // output routines refer to the file to be overwritten.

  float       getInputVe                ()  const;   // from pickle jar.
  float       getInputRef               ()  const;   // from pickle jar.
  float       getInputFixdist           ()  const;   // from pickle jar.
  int         getInputChaining          ()  const;   // from pickle jar.
  long        numInputLdCards           ()  const;   // from pickle jar.
  long        numInputRpCards           ()  const;   // from pickle jar.
  long        numInputPpCards           ()  const;   // from pickle jar.
  long        numInputZt1Cards          ()  const;   // from pickle jar.
  long        numInputZt2Cards          ()  const;   // from pickle jar.
  long        numInputZt3Cards          ()  const;   // from pickle jar.
  long        numInputZt4Cards          ()  const;   // from pickle jar.
  int         inputGridTransformStatus  ()  const;   // from pickle jar.

  int         getOutputChaining         ()  const;   // from pickle jar.
  long        numOutputLdCards          ()  const;   // from pickle jar.
  long        numOutputRpCards          ()  const;   // from pickle jar.
  long        numOutputPpCards          ()  const;   // from pickle jar.
  long        numOutputZt1Cards         ()  const;   // from pickle jar.
  long        numOutputZt2Cards         ()  const;   // from pickle jar.
  long        numOutputZt3Cards         ()  const;   // from pickle jar.
  long        numOutputZt4Cards         ()  const;   // from pickle jar.
  int         outputGridTransformStatus ()  const;   // from pickle jar.

  int    ldReadFlag ()  const  { return     _ld_read_flag; }
  int    rpReadFlag ()  const  { return     _rp_read_flag; }
  int    ppReadFlag ()  const  { return     _pp_read_flag; }
  int   zt1ReadFlag ()  const  { return    _zt1_read_flag; }
  int   zt2ReadFlag ()  const  { return    _zt2_read_flag; }
  int   zt3ReadFlag ()  const  { return    _zt3_read_flag; }
  int   zt4ReadFlag ()  const  { return    _zt4_read_flag; }
  int  gridReadFlag ()  const  { return   _grid_read_flag; }

  int   ldClearFlag ()  const  { return    _ld_clear_flag; }
  int   rpClearFlag ()  const  { return    _rp_clear_flag; }
  int   ppClearFlag ()  const  { return    _pp_clear_flag; }
  int  zt1ClearFlag ()  const  { return   _zt1_clear_flag; }
  int  zt2ClearFlag ()  const  { return   _zt2_clear_flag; }
  int  zt3ClearFlag ()  const  { return   _zt3_clear_flag; }
  int  zt4ClearFlag ()  const  { return   _zt4_clear_flag; }

  int    ldSaveFlag ()  const  { return     _ld_save_flag; }
  int    rpSaveFlag ()  const  { return     _rp_save_flag; }
  int    ppSaveFlag ()  const  { return     _pp_save_flag; }
  int   zt1SaveFlag ()  const  { return    _zt1_save_flag; }
  int   zt2SaveFlag ()  const  { return    _zt2_save_flag; }
  int   zt3SaveFlag ()  const  { return    _zt3_save_flag; }
  int   zt4SaveFlag ()  const  { return    _zt4_save_flag; }
  int  gridSaveFlag ()  const  { return   _grid_save_flag; }

  int  replaceSelectedMatchingLinesOrFlags()  const  { return _repsel; }
  int  replaceAnyMatchingLinesOrFlags     ()  const  { return _repany; }

public:   // set values (inline).

  void        setInputVe         (float ve      );   // to pickle jar.
  void        setInputRef        (float ref     );   // to pickle jar.
  void        setInputFixdist    (float fixdist );   // to pickle jar.
  void        setInputChaining   (int   chaining);   // to pickle jar.

  void    ldReadFlagSet (int value) {   _ld_read_flag = value; skipRead(); }
  void    rpReadFlagSet (int value) {   _rp_read_flag = value; skipRead(); }
  void    ppReadFlagSet (int value) {   _pp_read_flag = value; skipRead(); }
  void   zt1ReadFlagSet (int value) {  _zt1_read_flag = value; skipRead(); }
  void   zt2ReadFlagSet (int value) {  _zt2_read_flag = value; skipRead(); }
  void   zt3ReadFlagSet (int value) {  _zt3_read_flag = value; skipRead(); }
  void   zt4ReadFlagSet (int value) {  _zt4_read_flag = value; skipRead(); }
  void  gridReadFlagSet (int value) { _grid_read_flag = value; skipRead(); }

  void   ldClearFlagSet (int value) {  _ld_clear_flag = value; }
  void   rpClearFlagSet (int value) {  _rp_clear_flag = value; }
  void   ppClearFlagSet (int value) {  _pp_clear_flag = value; }
  void  zt1ClearFlagSet (int value) { _zt1_clear_flag = value; }
  void  zt2ClearFlagSet (int value) { _zt2_clear_flag = value; }
  void  zt3ClearFlagSet (int value) { _zt3_clear_flag = value; }
  void  zt4ClearFlagSet (int value) { _zt4_clear_flag = value; }

  void    ldSaveFlagSet (int value) {   _ld_save_flag = value; updatePjar(); }
  void    rpSaveFlagSet (int value) {   _rp_save_flag = value; updatePjar(); }
  void    ppSaveFlagSet (int value) {   _pp_save_flag = value; updatePjar(); }
  void   zt1SaveFlagSet (int value) {  _zt1_save_flag = value; updatePjar(); }
  void   zt2SaveFlagSet (int value) {  _zt2_save_flag = value; updatePjar(); }
  void   zt3SaveFlagSet (int value) {  _zt3_save_flag = value; updatePjar(); }
  void   zt4SaveFlagSet (int value) {  _zt4_save_flag = value; updatePjar(); }
  void  gridSaveFlagSet (int value) { _grid_save_flag = value; updatePjar(); }

  void replaceSelectedMatchingLinesOrFlags(int value)  { _repsel = value;
                                                         _repany = 0; }
  void replaceAnyMatchingLinesOrFlags     (int value)  { _repany = value;
                                                         _repsel = 0; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
