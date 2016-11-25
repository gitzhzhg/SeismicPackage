package org.cpseis.util;

/**
Java wrapper around the parameter cache primitive.
See the parameter cache primitive for details.
*/

//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//
//---------------------------- start of class ----------------------------//

public class PC
{
                                                                                
//-------------------------------- data ----------------------------------//
//-------------------------------- data ----------------------------------//
//-------------------------------- data ----------------------------------//
                                                                                
public static final int FRONTEND         =   1;    // update state.
public static final int GUI              =   2;    // update state.
public static final int BACKEND          =   3;    // update state.
public static final int EXECUTE          =   4;    // update state.
public static final int QUICK            =   5;    // update state.
public static final int BACKEND_NO_EXEC  =   6;    // update state
                                                                                
public static final int INSERT           =   1;    // array element action.
public static final int REMOVE           =   2;    // array element action.
public static final int MODIFY           =   3;    // array element action.
public static final int NOACTION         =   4;    // array element action.
                                                                                
public static final int MISSING          =   1;    // nature of parameter.
public static final int SCALAR           =   2;    // nature of parameter.
public static final int ARRAY            =   3;    // nature of parameter.
                                                                                
public static final int INTEGER          =   1;    // vartype of parameter.
public static final int FLOAT            =   2;    // vartype of parameter.
public static final int DOUBLE           =   3;    // vartype of parameter.
public static final int STRING           =   4;    // vartype of parameter.
public static final int LOGICAL          =   5;    // vartype of parameter.
public static final int GRID             =   6;    // vartype of parameter.
                                                                                
public static final int DATACARD_LENGTH  =  80;    // datacard length.
public static final int DATACARD_PADDING =   8;    // datacard padding.
public static final int LENGTH           = 160; // scalar or array elem length.
public static final int KEYWORD_LENGTH   =  80;    // keyword length.
 
//-------------------------- native prototypes ----------------------------//
//-------------------------- native prototypes ----------------------------//
//-------------------------- native prototypes ----------------------------//

static { System.loadLibrary("org_cpseis_util_library"); }

public static native boolean  exists();

public static native void     frontendUpdate ();
public static native void     backendUpdate  ();
public static native void     guiUpdate      ();
public static native void     quickUpdate    ();

public static native void     frontendUpdateNoprint ();
public static native void     backendUpdateNoprint  ();
public static native void     guiUpdateNoprint      ();
public static native void     quickUpdateNoprint    ();

public static native void     clear                 ();
public static native void     restore               ();
public static native void     next                  ();
public static native void     backendExecute        ();
public static native void     continueBackendUpdate ();

public static native int      getUpdateState  (); // returns update state enum.
public static native void     setBackendNoExec   ();
public static native void     setBackendYesExec  ();
public static native int      getIpn             ();
public static native boolean  previousError      ();
public static native void     setIpn             (int ipn);

public static native boolean  doNotProcessTraces ();

public static native boolean  updateError        ();
public static native void     error              (String msg);
public static native void     warning            (String msg);
public static native void     info               (String msg);
public static native void     print              (String msg);

                       ///////////////////////////////

public static native void printAllDataCards  ();

public static native void printProcessCards  ();
public static native void printGlobalCards   ();
public static native void printControlCards  ();
public static native void printPdataCards    ();
public static native void printJdataCards    ();
public static native void printGuiCards      ();

public static native void infoProcessCards   ();
public static native void infoGlobalCards    ();
public static native void infoControlCards   ();
public static native void infoPdataCards     ();
public static native void infoJdataCards     ();
public static native void infoGuiCards       ();

                       ///////////////////////////////

                  // key is shorthand for keyword.
                  // act is shorthand for action.
                  // def is shorthand for default_scalar or default_array.

public static native int  numElementsProcess (String key);
public static native int  numElementsGlobal  (String key);
public static native int  numElementsControl (String key);
public static native int  numElementsGui     (String key, String act);
public static native int  numElementsPdata   (String key);
public static native int  numElementsJdata   (String key);

       // these return nature enum:

public static native int  natureProcess      (String key);
public static native int  natureGlobal       (String key);
public static native int  natureControl      (String key);
public static native int  natureGui          (String key, String act);
public static native int  naturePdata        (String key);
public static native int  natureJdata        (String key);

       // these return vartype enum:

public static native int  vartypeProcess     (String key);
public static native int  vartypeGlobal      (String key);
public static native int  vartypeControl     (String key);
public static native int  vartypeGui         (String key, String act);
public static native int  vartypePdata       (String key);
public static native int  vartypeJdata       (String key);

                       ///////////////////////////////

public static native GridTransform get (String key, GridTransform def);
public static native int           get (String key, int           def);
public static native float         get (String key, float         def);
public static native double        get (String key, double        def);
public static native boolean       get (String key, boolean       def);
public static native String        get (String key, String        def);

public static native GridTransform getProcess  (String key, GridTransform def);
public static native int           getProcess  (String key, int           def);
public static native float         getProcess  (String key, float         def);
public static native double        getProcess  (String key, double        def);
public static native boolean       getProcess  (String key, boolean       def);
public static native String        getProcess  (String key, String        def);

public static native GridTransform getGlobal   (String key, GridTransform def);
public static native int           getGlobal   (String key, int           def);
public static native float         getGlobal   (String key, float         def);
public static native double        getGlobal   (String key, double        def);
public static native boolean       getGlobal   (String key, boolean       def);
public static native String        getGlobal   (String key, String        def);

public static native GridTransform getControl  (String key, GridTransform def);
public static native int           getControl  (String key, int           def);
public static native float         getControl  (String key, float         def);
public static native double        getControl  (String key, double        def);
public static native boolean       getControl  (String key, boolean       def);
public static native String        getControl  (String key, String        def);

public static native GridTransform getGui
                                    (String key, String act, GridTransform def);
public static native int           getGui (String key, String act, int     def);
public static native float         getGui (String key, String act, float   def);
public static native double        getGui (String key, String act, double  def);
public static native boolean       getGui (String key, String act, boolean def);
public static native String        getGui (String key, String act, String  def);

public static native GridTransform getPdata    (String key, GridTransform def);
public static native int           getPdata    (String key, int           def);
public static native float         getPdata    (String key, float         def);
public static native double        getPdata    (String key, double        def);
public static native boolean       getPdata    (String key, boolean       def);
public static native String        getPdata    (String key, String        def);

public static native GridTransform getJdata    (String key, GridTransform def);
public static native int           getJdata    (String key, int           def);
public static native float         getJdata    (String key, float         def);
public static native double        getJdata    (String key, double        def);
public static native boolean       getJdata    (String key, boolean       def);
public static native String        getJdata    (String key, String        def);

                       ///////////////////////////////

public static native int[]     get         (String key, int     def[]);
public static native float[]   get         (String key, float   def[]);
public static native double[]  get         (String key, double  def[]);
public static native boolean[] get         (String key, boolean def[]);
public static native String[]  get         (String key, String  def[]);

public static native int[]     getProcess  (String key, int     def[]);
public static native float[]   getProcess  (String key, float   def[]);
public static native double[]  getProcess  (String key, double  def[]);
public static native boolean[] getProcess  (String key, boolean def[]);
public static native String[]  getProcess  (String key, String  def[]);

public static native int[]     getGlobal   (String key, int     def[]);
public static native float[]   getGlobal   (String key, float   def[]);
public static native double[]  getGlobal   (String key, double  def[]);
public static native boolean[] getGlobal   (String key, boolean def[]);
public static native String[]  getGlobal   (String key, String  def[]);

public static native int[]     getControl  (String key, int     def[]);
public static native float[]   getControl  (String key, float   def[]);
public static native double[]  getControl  (String key, double  def[]);
public static native boolean[] getControl  (String key, boolean def[]);
public static native String[]  getControl  (String key, String  def[]);

public static native int[]     getGui  (String key, String act, int     def[]);
public static native float[]   getGui  (String key, String act, float   def[]);
public static native double[]  getGui  (String key, String act, double  def[]);
public static native boolean[] getGui  (String key, String act, boolean def[]);
public static native String[]  getGui  (String key, String act, String  def[]);

public static native int[]     getPdata    (String key, int     def[]);
public static native float[]   getPdata    (String key, float   def[]);
public static native double[]  getPdata    (String key, double  def[]);
public static native boolean[] getPdata    (String key, boolean def[]);
public static native String[]  getPdata    (String key, String  def[]);

public static native int[]     getJdata    (String key, int     def[]);
public static native float[]   getJdata    (String key, float   def[]);
public static native double[]  getJdata    (String key, double  def[]);
public static native boolean[] getJdata    (String key, boolean def[]);
public static native String[]  getJdata    (String key, String  def[]);

                       ///////////////////////////////

public static native int     getProcessElementInt     (String key, int indx);
public static native float   getProcessElementFloat   (String key, int indx);
public static native double  getProcessElementDouble  (String key, int indx);
public static native boolean getProcessElementBoolean (String key, int indx);
public static native String  getProcessElementString  (String key, int indx);

public static native int     getGlobalElementInt      (String key, int indx);
public static native float   getGlobalElementFloat    (String key, int indx);
public static native double  getGlobalElementDouble   (String key, int indx);
public static native boolean getGlobalElementBoolean  (String key, int indx);
public static native String  getGlobalElementString   (String key, int indx);

public static native int     getControlElementInt     (String key, int indx);
public static native float   getControlElementFloat   (String key, int indx);
public static native double  getControlElementDouble  (String key, int indx);
public static native boolean getControlElementBoolean (String key, int indx);
public static native String  getControlElementString  (String key, int indx);

public static native int     getGuiElementInt     
                                           (String key, String act, int indx);
public static native float   getGuiElementFloat   
                                           (String key, String act, int indx);
public static native double  getGuiElementDouble  
                                           (String key, String act, int indx);
public static native boolean getGuiElementBoolean 
                                           (String key, String act, int indx);
public static native String  getGuiElementString  
                                           (String key, String act, int indx);

public static native int     getPdataElementInt       (String key, int indx);
public static native float   getPdataElementFloat     (String key, int indx);
public static native double  getPdataElementDouble    (String key, int indx);
public static native boolean getPdataElementBoolean   (String key, int indx);
public static native String  getPdataElementString    (String key, int indx);

public static native int     getJdataElementInt       (String key, int indx);
public static native float   getJdataElementFloat     (String key, int indx);
public static native double  getJdataElementDouble    (String key, int indx);
public static native boolean getJdataElementBoolean   (String key, int indx);
public static native String  getJdataElementString    (String key, int indx);

                       ///////////////////////////////

public static native boolean  pressed    (String key);
public static native String   activated  ();
                                  // returns keyword which was activated.

public static native boolean  verifyScalar      (String key);
public static native boolean  verifyElement     (String key);
public static native int verifyElementGetIndex  (String key); // returns index.
public static native int verifyElementGetAction (String key); // returns action.
public static native boolean  verifyArray       (String key);
public static native boolean  verifyArrayset    (String key);
public static native boolean  verifyScreen      (String key);
public static native boolean  verifyEnd         ();

                       ///////////////////////////////

public static native void put         (String key, GridTransform scalar);
public static native void put         (String key, int           scalar);
public static native void put         (String key, float         scalar);
public static native void put         (String key, double        scalar);
public static native void put         (String key, boolean       scalar);
public static native void put         (String key, String        scalar);

public static native void putProcess  (String key, GridTransform scalar);
public static native void putProcess  (String key, int           scalar);
public static native void putProcess  (String key, float         scalar);
public static native void putProcess  (String key, double        scalar);
public static native void putProcess  (String key, boolean       scalar);
public static native void putProcess  (String key, String        scalar);

public static native void putGlobal   (String key, GridTransform scalar);
public static native void putGlobal   (String key, int           scalar);
public static native void putGlobal   (String key, float         scalar);
public static native void putGlobal   (String key, double        scalar);
public static native void putGlobal   (String key, boolean       scalar);
public static native void putGlobal   (String key, String        scalar);

public static native void putControl  (String key, GridTransform scalar);
public static native void putControl  (String key, int           scalar);
public static native void putControl  (String key, float         scalar);
public static native void putControl  (String key, double        scalar);
public static native void putControl  (String key, boolean       scalar);
public static native void putControl  (String key, String        scalar);

public static native void putGui (String key, String act, GridTransform scalar);
public static native void putGui (String key, String act, int           scalar);
public static native void putGui (String key, String act, float         scalar);
public static native void putGui (String key, String act, double        scalar);
public static native void putGui (String key, String act, boolean       scalar);
public static native void putGui (String key, String act, String        scalar);

public static native void putGuiOnly  (String key, GridTransform scalar);
public static native void putGuiOnly  (String key, int           scalar);
public static native void putGuiOnly  (String key, float         scalar);
public static native void putGuiOnly  (String key, double        scalar);
public static native void putGuiOnly  (String key, boolean       scalar);
public static native void putGuiOnly  (String key, String        scalar);

public static native void putPdata    (String key, GridTransform scalar);
public static native void putPdata    (String key, int           scalar);
public static native void putPdata    (String key, float         scalar);
public static native void putPdata    (String key, double        scalar);
public static native void putPdata    (String key, boolean       scalar);
public static native void putPdata    (String key, String        scalar);

public static native void putJdata    (String key, GridTransform scalar);
public static native void putJdata    (String key, int           scalar);
public static native void putJdata    (String key, float         scalar);
public static native void putJdata    (String key, double        scalar);
public static native void putJdata    (String key, boolean       scalar);
public static native void putJdata    (String key, String        scalar);

                       ///////////////////////////////

public static native void put         (String key, int     array[]);
public static native void put         (String key, float   array[]);
public static native void put         (String key, double  array[]);
public static native void put         (String key, boolean array[]);
public static native void put         (String key, String  array[]);

public static native void putProcess  (String key, int     array[]);
public static native void putProcess  (String key, float   array[]);
public static native void putProcess  (String key, double  array[]);
public static native void putProcess  (String key, boolean array[]);
public static native void putProcess  (String key, String  array[]);

public static native void putGlobal   (String key, int     array[]);
public static native void putGlobal   (String key, float   array[]);
public static native void putGlobal   (String key, double  array[]);
public static native void putGlobal   (String key, boolean array[]);
public static native void putGlobal   (String key, String  array[]);

public static native void putControl  (String key, int     array[]);
public static native void putControl  (String key, float   array[]);
public static native void putControl  (String key, double  array[]);
public static native void putControl  (String key, boolean array[]);
public static native void putControl  (String key, String  array[]);

public static native void putGui (String key, String act, int     array[]);
public static native void putGui (String key, String act, float   array[]);
public static native void putGui (String key, String act, double  array[]);
public static native void putGui (String key, String act, boolean array[]);
public static native void putGui (String key, String act, String  array[]);

public static native void putGuiOnly  (String key, int     array[]);
public static native void putGuiOnly  (String key, float   array[]);
public static native void putGuiOnly  (String key, double  array[]);
public static native void putGuiOnly  (String key, boolean array[]);
public static native void putGuiOnly  (String key, String  array[]);

public static native void putPdata    (String key, int     array[]);
public static native void putPdata    (String key, float   array[]);
public static native void putPdata    (String key, double  array[]);
public static native void putPdata    (String key, boolean array[]);
public static native void putPdata    (String key, String  array[]);

public static native void putJdata    (String key, int     array[]);
public static native void putJdata    (String key, float   array[]);
public static native void putJdata    (String key, double  array[]);
public static native void putJdata    (String key, boolean array[]);
public static native void putJdata    (String key, String  array[]);

                       ///////////////////////////////

public static native void registerArrayNames (String key, String arrays[]);

public static native void putOptions   (String key, int     options[]);
public static native void putOptions   (String key, float   options[]);
public static native void putOptions   (String key, double  options[]);
public static native void putOptions   (String key, boolean options[]);
public static native void putOptions   (String key, String  options[]);

public static native void putOptionsA  (String key, int     options[]);
public static native void putOptionsA  (String key, float   options[]);
public static native void putOptionsA  (String key, double  options[]);
public static native void putOptionsA  (String key, boolean options[]);
public static native void putOptionsA  (String key, String  options[]);

public static native void putSensitiveFieldFlag    (String key, boolean sense);
public static native void putSensitiveArrayFlag    (String key, boolean sense);
public static native void putSensitiveArraysetFlag (String key, boolean sense);
public static native void putSensitiveScreenFlag   (String key, boolean sense);

public static native void putVisibleFlag         (String key, boolean visible);

public static native void putMinsizeArray          (String key, int minsize);
public static native void putMinsizeArrayset       (String key, int minsize);
public static native void putMaxsizeArray          (String key, int maxsize);
public static native void putMaxsizeArrayset       (String key, int maxsize);

                       ///////////////////////////////

public static native int  numProcessCards ();
public static native int  numGlobalCards  ();
public static native int  numControlCards ();
public static native int  numPdataCards   ();
public static native int  numJdataCards   ();
public static native int  numGuiCards     ();

                       ///////////////////////////////

public static native String[] getProcessCards();
public static native String[] getGlobalCards ();
public static native String[] getControlCards();
public static native String[] getPdataCards  ();
public static native String[] getJdataCards  ();
public static native String[] getGuiCards    ();

public static native String getProcessCard  (int icard);
public static native String getGlobalCard   (int icard);
public static native String getControlCard  (int icard);
public static native String getPdataCard    (int icard);
public static native String getJdataCard    (int icard);
public static native String getGuiCard      (int icard);

public static native void putProcessCards (String      cards[]);
public static native void putGlobalCards  (String      cards[]);
public static native void putControlCards (String      cards[]);
public static native void putPdataCards   (String      cards[]);
public static native void putJdataCards   (String      cards[]);
public static native void putGuiCards     (String      cards[]);

public static native void putProcessCard  (String      card);
public static native void putGlobalCard   (String      card);
public static native void putControlCard  (String      card);
public static native void putPdataCard    (String      card);
public static native void putJdataCard    (String      card);
public static native void putGuiCard      (String      card);

public static native void addProcessCard  (String      card);
public static native void addGlobalCard   (String      card);
public static native void addControlCard  (String      card);
public static native void addPdataCard    (String      card);
public static native void addJdataCard    (String      card);
public static native void addGuiCard      (String      card);

                       ///////////////////////////////

public static native void clearProcessCards     ();
public static native void clearGlobalCards      ();
public static native void clearControlCards     ();
public static native void clearPdataCards       ();
public static native void clearJdataCards       ();
public static native void clearGuiCards         ();

public static native boolean  processKeywordPresent (String key);
public static native boolean  globalKeywordPresent  (String key);
public static native boolean  controlKeywordPresent (String key);
public static native boolean  pdataKeywordPresent   (String key);
public static native boolean  jdataKeywordPresent   (String key);
public static native boolean  guiActionPresent      (String key, String act);

public static native int  numProcessKeywords    ();
public static native int  numGlobalKeywords     ();
public static native int  numControlKeywords    ();
public static native int  numPdataKeywords      ();
public static native int  numJdataKeywords      ();
public static native int  numGuiKeywords        ();

public static native String getProcessKeyword     (int indx);
public static native String getGlobalKeyword      (int indx);
public static native String getControlKeyword     (int indx);
public static native String getPdataKeyword       (int indx);
public static native String getJdataKeyword       (int indx);
public static native String getGuiKeyword         (int indx);
public static native String getGuiAction          (int indx);

public static native void removeProcessKeyword  (String key);
public static native void removeGlobalKeyword   (String key);
public static native void removeControlKeyword  (String key);
public static native void removePdataKeyword    (String key);
public static native void removeJdataKeyword    (String key);
public static native void removeGuiAction       (String key, String act);

//------------------------- get keywords -------------------------------//
//------------------------- get keywords -------------------------------//
//------------------------- get keywords -------------------------------//

public static String[] getProcessKeywords()
{
  int num = numProcessKeywords();
  String[] keys = new String[num];
  for(int i = 0; i < num; i++)
      {
      keys[i] = getProcessKeyword(i);
      }
  return keys;
}

public static String[] getGlobalKeywords()
{
  int num = numGlobalKeywords();
  String[] keys = new String[num];
  for(int i = 0; i < num; i++)
      {
      keys[i] = getProcessKeyword(i);
      }
  return keys;
}

public static String[] getControlKeywords()
{
  int num = numControlKeywords();
  String[] keys = new String[num];
  for(int i = 0; i < num; i++)
      {
      keys[i] = getProcessKeyword(i);
      }
  return keys;
}

public static String[] getPdataKeywords()
{
  int num = numPdataKeywords();
  String[] keys = new String[num];
  for(int i = 0; i < num; i++)
      {
      keys[i] = getProcessKeyword(i);
      }
  return keys;
}

public static String[] getJdataKeywords()
{
  int num = numJdataKeywords();
  String[] keys = new String[num];
  for(int i = 0; i < num; i++)
      {
      keys[i] = getProcessKeyword(i);
      }
  return keys;
}

public static String[] getGuiKeywords()
{
  int num = numGuiKeywords();
  String[] keys = new String[num];
  for(int i = 0; i < num; i++)
      {
      keys[i] = getGuiKeyword(i);
      }
  return keys;
}

public static String[] getGuiActions()
{
  int num = numGuiKeywords();
  String[] actions = new String[num];
  for(int i = 0; i < num; i++)
      {
      actions[i] = getGuiAction(i);
      }
  return actions;
}

//------------------------- put messages -------------------------------//
//------------------------- put messages -------------------------------//
//------------------------- put messages -------------------------------//

public static void putErrors(String... msgs)
{
  for(String msg : msgs) error(msg);
}

public static void putWarnings(String... msgs)
{
  for(String msg : msgs) warning(msg);
}

public static void putInfos(String... msgs)
{
  for(String msg : msgs) info(msg);
}

//------------------------- get messages -------------------------------//
//------------------------- get messages -------------------------------//
//------------------------- get messages -------------------------------//

public static String[] getErrors()
{
  return getGui("ERROR", "ERROR", new String[0]);
}

public static String[] getWarnings()
{
  return getGui("WARNING", "WARNING", new String[0]);
}

public static String[] getInfos()
{
  return getGui("INFO", "INFO", new String[0]);
}

//--------------------------- end of class -------------------------------//
//--------------------------- end of class -------------------------------//
//--------------------------- end of class -------------------------------//
                                                                                
}
                                                                                
//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
//-------------------------------- end ---------------------------------//
