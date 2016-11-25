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

#ifndef _FileChoice_h
#define _FileChoice_h

#include <Xm/Xm.h>
#include "wproc.h"

#define XmNannoType                 "annoType"
#define XmNerrorWidget              "errorWidget"
#define XmNfilename                 "filename"
#define XmNfileTargetAddr           "fileTargetAddr"
#define XmNfileDescription          "fileDescription"
#define XmNfileExtension            "fileExtension"
#define XmNfileFlags                "fileFlags"
#define XmNtextOffset               "textOffset"
#define XmNalignFileChoiceWidget    "alignFileChoiceWidget"
#define XmNalignOtherWidget         "alignOtherWidget"
#define XmNpercentRight             "percentRight"
#define XmNoffsetRight              "offsetRight"
#define XmNtextAlignType            "textAlignType"
#define XmNuseNoneStr               "useNoneStr"
#define XmNnoneStr                  "noneStr"
#define XmNvalidateOnSelection      "validateOnSelection"

#define XmNsuccessCallback          "successCallback"
#define XmNfailCallback             "failCallback"
#define XmNfileSelCallback          "fileSelCallback"



#define XmCAnnoType                 "AnnoType"
#define XmCErrorWidget              "ErrorWidget"
#define XmCFilename                 "Filename"
#define XmCFileTargetAddr           "FileTargetAddr"
#define XmCTextOffset               "TextOffset"
#define XmCFileExtension            "FileExtension"
#define XmCFileFlags                "FileFlags"
#define XmCFileDescription          "FileDescription"
#define XmCAlignFileChoiceWidget    "AlignFileChoiceWidget"
#define XmCAlignOtherWidget         "AlignOtherWidget"
#define XmCPercentRight             "PercentRight"
#define XmCOffsetRight              "OffsetRight"
#define XmCTextAlignType            "TextAlignType"
#define XmCUseNoneStr               "UseNoneStr"
#define XmCNoneStr                  "NoneStr"
#define XmCValidateOnSelection      "ValidateOnSelection"


#define XmCSuccessCallback          "SuccessCallback"
#define XmCFailCallback             "FailCallback"
#define XmCFileSelCallback          "FileSelCallback"

/*
 *     ----------------------------------------------
 */
#define  wprocRAnnoType      "wprocAnnoType"
#define  wprocRAlignType     "wprocAlignType"
#define  wprocRFileFlgType   "wprocFileFlgType"

typedef long wprocAnnoType;
typedef long wprocAlignType;
typedef long wprocFileFlgType;

#define wprocLabel       1
#define wprocPushButton  2
#define wprocNoAnno      3

#define wprocLabelStr       "LABEL"
#define wprocPushButtonStr  "PUSHBUTTON"
#define wprocNoAnnoStr      "NOANNO"


#define wprocOffsetAnno          1
#define wprocOffsetAuto          2
#define wprocOffsetRight         3
#define wprocOffsetWithWidget    4
#define wprocOffsetWithFCWidget  5

#define wprocOffsetAnnoStr          "OFFSETANNO"
#define wprocOffsetAutoStr          "OFFSETAUTO"
#define wprocOffsetRightStr         "OFFSETRIGHT"
#define wprocOffsetWithWidgetStr    "OFFSETWITHWIDGET"
#define wprocOffsetWithFCWidgetStr  "OFFSETWITHFCWIDGET"


#define wprocOffsetAnno          1
#define wprocOffsetAuto          2
#define wprocOffsetRight         3
#define wprocOffsetWithWidget    4
#define wprocOffsetWithFCWidget  5



#define wprocMustExistMask         FI_MUST_EXIST 
#define wprocIsRequiredMask        FI_IS_REQUIRED
#define wprocWritableMask          FI_WRITABLE  
#define wprocMsgBlkMask            FI_NOMSG_BLK
#define wprocIgnoreFocMask         FI_IGNORE_FOC
#define wprocNoAlwaysActChkMask    FI_NO_ALWAYS_ACT_CHK
#define wprocAddExtAlwaysMask      FI_ADD_EXT_ALWAYS
#define wprocOverWriteWarnMask     FI_OVERWRT_WARN
#define wprocExpandFileMask        FI_EXPAND_FILE
#define wprocSqueezeBlanksMask     FI_SQUEEZE_BLANKS

#define wprocNoDetail             False
#define wprocDoesNotExist         1
#define wprocWasRequired          2
#define wprocNotWritable          3
#define wprocWillOverWrite        4

#define wprocMustExistStr         "MUSTEXIST"
#define wprocIsRequiredStr        "ISREQUIRED"
#define wprocWritableStr          "WRITABLE"
#define wprocMsgBlkStr            "MSGBLK"
#define wprocIgnoreFocStr         "IGNOREFOC"
#define wprocNoAlwaysActChkStr    "NOALWAYSACTCHK"
#define wprocAddExtAlwaysStr      "ADDEXTALWAYS"
#define wprocOverWriteWarnStr     "OVERWRITEWARN"
#define wprocExpandFileStr        "EXPANDFILE"
#define wprocSqueezeBlanksStr     "SQUEEZEBLANKS"



typedef struct _FileChoiceClassRec *FileChoiceWidgetClass;
typedef struct _FileChoiceRec      *FileChoiceWidget;



typedef struct _wprocFileChoiceCallbackStruct {
           int                  reason;
           char                 *filename;
           XmAnyCallbackStruct  *tw;
           long                 detail;
           Boolean              doit;
           char                 *sav_filename;
           unsigned long        check_flags;
           char                 *fail_str;
  } wprocFileChoiceSuccCallbackStruct, *wprocFileChoiceSuccCallbackPtr, 
    wprocFileChoiceFailCallbackStruct, *wprocFileChoiceFailCallbackPtr,
    wprocFileChoiceCallbackStruct, *wprocFileChoicelCallbackPtr;

typedef struct _wprocFileChoiceFileSelCallbackStruct {
           int                   reason;
           char                 *filename;
           XmAnyCallbackStruct  *sbw;
  } wprocFileChoiceFileSelCallbackStruct, *wprocFileChoiceFileSelCallbackPtr;

#define wprocSuccessCallbackReason 5000
#define wprocFileSelCallbackReason 5001
#define wprocFailCallbackReason    5002


#ifdef __cplusplus  
extern "C" {                          /* for C++ */
#endif
/*
 * Declaration of public functions
 */
void wprocFileChoiceShowErr( Widget w, char   *errstr );

void wprocFileChoiceSetFile( Widget  w,
                             char    *filename,
                             Boolean docheck     );

char *wprocFileChoiceGetFile( Widget  w );

Boolean wprocFileChoiceGoodFile( Widget  w );

Boolean wprocFileChoiceGetFileByStr( Widget  w, char   *filename );

Boolean wprocFileChoiceValidate( Widget  w, Boolean evenif  );

void wprocFileChoiceRetWidgets( Widget  w,
                                Widget  *textw,
                                Widget  *annow,
                                Widget  *fsbpopw,
                                Widget  *errpop );

Boolean wprocFileChoiceValidate( Widget  w, Boolean evenif  );


unsigned long wprocFileChoiceGetFlags( Widget  w );

unsigned long wprocFileChoiceSetFlags( Widget        w,
                                       unsigned long flgs );

void wprocFileChoiceSBSetPath( Widget w, char   *path );

char *wprocFileChoiceSBGetPath( Widget w);

void wprocFileChoiceSBSetDir( Widget w, char  *dir );
char *wprocFileChoiceSBGetDir( Widget     w);

void wprocFileChoiceSetDirectoryUpdate(Boolean do_update);
Boolean wprocFileChoiceGetDirectoryUpdate();
void wprocFileChoiceSetAltFileAccess(Widget w, AltFileAccess *net_file);



#ifdef __cplusplus  
}
#endif

extern WidgetClass fileChoiceWidgetClass;


#endif  /* _FileChoice_h */
/* DON'T ADD STUFF AFTER THIS #endif */
