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

//-------------------------- how_read_file.hh ------------------------------//
//-------------------------- how_read_file.hh ------------------------------//
//-------------------------- how_read_file.hh ------------------------------//

//                header file for the HowReadFile class
//                     not derived from any class
//                        subdirectory oprim


           // This class contains information needed to
           // read self-defining files, including foreign
           // files with missing information.

// ENCODING:
//  (1) encoding format of file (either "ascii" or "binary").
//  (2) default is "ascii".
//  (3) must always be "ascii" for foreign files.
//
// FIELDS[NFIELDS]:
//  (1) list of fields (names of columns) to read or write.
//  (2) default is NFIELDS = 0.
//  (3) must always be specified for one or more fields.
//
// NILSTRING:
//  (1) string representing nil values in the file.
//  (2) default is "nil".
//
// WRAP:
//  (1) number of card images per record.
//  (2) default is 1.
//  (3) useful for files with wrapped lines, where more than one card image
//       contributes to a set of fields to be decoded.
//  (4) wrapping the lines keeps individual lines from being too long for
//       easier reading in a text editor.
//  (5) LAS files are sometimes wrapped files.
//
// FIRSTLINE:
//  (1) line to start reading on.
//  (2) default is 1.
//  (3) useful for files which contain header information without # at the
//       beginning of the line.
//  (4) LAS files are an example of this kind of file.
//
// TEMPLAT:
//  (1) template for adjusting foreign input card image.
//  (2) default is " " (a blank character string).
//  (3) if WRAP > 1, this parameter is used on only the first card image.
//  (4) a hyphen character (-) at a specified location means that the
//       corresponding character location (character number in card image)
//       should be ignored, and will be set to a space character.
//  (5) a non-blank character (other than a hyphen) at a specified location
//       means that a new field, which might not be separated from the previous
//       field, starts at the corresponding character location of the card
//       image.
//  (6) can be used when two adjacent fields are not separated by a space, and
//       the beginning of the second of the two juxtaposed fields always begins
//       at the same character location in all card images.
//  (7) can be used when there is a region of undesirable information, possibly
//       broken into variable numbers of blank-delimited fields, which always
//       resides at the same character location in all card images.
//  (8) commas are always ignored, even when WRAP > 1 or the template is blank.
//  (9) example for desired fields aaaa, bbbbb, ccccc, and undesirable field xx:
//       original card image:     " xxx x aaaabbbbb, ccccc     "
//       template:                " -----     x                "
//       modified card image:     "       aaaa bbbbb  ccccc    "
//
// MAXCHARS[NMAXCHARS]:
//  (1) maximum number of characters in each field to decode.
//  (2) default is NMAXCHARS = 0.
//  (3) zero means to use all of the characters in the field with no maximum.
//  (4) positive or negative means to use no more than this many characters.
//  (5) positive means the rest of the field becomes the next field.
//  (6) negative means the rest of the field is skipped.
//  (7) can be used when two adjacent fields are not separated by a space, and
//       the beginning of the second of the two juxtaposed fields always begins
//       after the same number of characters in the combined field.
//  (8) works for all values of WRAP.
//  (9) the fields do not have to begin at the same character location on
//       different card images.
// (10) example for desired fields aaaa, bbbbb, ccccc, ddd, ee, and fff:
//       original or modified card image:  " aaaabbbbb  cccccwww ddd  eefffgg "
//       maxchars:                            4    0     -5       0   2 -3
//       retained fields:                    aaaa bbbbb  ccccc   ddd  ee fff



//-------------------------- start of coding ----------------------------//
//-------------------------- start of coding ----------------------------//
//-------------------------- start of coding ----------------------------//

#ifndef _HOW_READ_FILE_HH_
#define _HOW_READ_FILE_HH_


class HowReadFile
{

//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//
//------------------------------- data ----------------------------------//

private:

  char  *_encoding  ;   // encoding format ("ascii" or "binary").
  char  *_nilstring ;   // string representing nil values (cannot be blank).
  char  *_templat   ;   // template for adjusting foreign card (can be blank).
  long   _wrap      ;   // number of wrapped lines (lines per record) (>=1).
  long   _firstline ;   // first line to read                         (>=1).
  long   _nfields   ;   // number of fields   array elements          (>=1).
  long   _nmaxchars ;   // number of maxchars array elements          (>=0).
  char **_fields    ;   // array of fields   (names of columns).
  long  *_maxchars  ;   // array of maxchars (maximum #characters to decode).

//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//
//----------------------------- functions -------------------------------//

public:

           HowReadFile ();
  virtual ~HowReadFile ();
  void     clear       ();    // resets all values to defaults.


  const char  *getEncoding  ()          const  { return _encoding ; }
  const char  *getNilstring ()          const  { return _nilstring; }
  const char  *getTemplat   ()          const  { return _templat  ; }
  long         getWrap      ()          const  { return _wrap     ; }
  long         getFirstline ()          const  { return _firstline; }
  long         getNfields   ()          const  { return _nfields  ; }
  long         getNmaxchars ()          const  { return _nmaxchars; }
  const char  *getField     (int indx)  const;
  long         getMaxchars  (int indx)  const;
  char* const *getFields    ()          const  { return _fields   ; }
  long        *getMaxchars  ()          const  { return _maxchars ; }


  void         setEncoding  (const char  *encoding                );
  void         setNilstring (const char  *nilstring               );
  void         setTemplat   (const char  *templat                 );
  void         setWrap      (long         wrap                    );
  void         setFirstline (long         firstline               );
  void         setNfields   (long         nfields                 );
  void         setNmaxchars (long         nmaxchars               );
  void         setField     (const char  *field    , int indx     );
  void         setMaxchars  (long         maxchars , int indx     );
  void         setFields    (char* const *fields   , int nfields  );
  void         setMaxchars  (long        *maxchars , int nmaxchars);

  // Alternative to calling setFields(fields,nfields):
  //   Call setNfields(nfields) to reset the number of fields and set
  //     them all to blank.
  //   Then call setField(field,indx) for each individual field.
  //   indx must be >= 0 and < nfields.

  // Alternative to calling setMaxchars(maxchars,nmaxchars):
  //   Call setNmaxchars(nmaxchars) to reset the number of maxchars and set
  //     them all to zero.
  //   Then call setMaxchars(maxchars,indx) for each individual maxchars.
  //   indx must be >= 0 and < nmaxchars.

//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//
//-------------------------- end of functions ---------------------------//

} ;

#endif

//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
//-------------------------------- end ------------------------------------//
