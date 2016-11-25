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
#ifndef SEIS_XY_HEADERS_HH
#define SEIS_XY_HEADERS_HH

class SeisXYHeaders {

public:
  SeisXYHeaders () {}				// constructor

  virtual ~SeisXYHeaders () {}			// destructor

  enum
    {XHEADER, 					// flag for X-header
     YHEADER, 					// flag for Y-header
     HEADER_SEVEN = 6, 				// 0-rel index to hdr wd 7
     HEADER_EIGHT = 7, 				// 0-rel index to hdr wd 8
     HEADER_SEVENTEEN = 16, 			// 0-rel index to hdr wd 17
     HEADER_EIGHTEEN = 17, 			// 0-rel index to hdr wd 18
     HEADER_THIRTYSEVEN = 36, 			// 0-rel index to hdr wd 37
     HEADER_THIRTYEIGHT = 37}; 			// 0-rel index to hdr wd 38

  enum
    {SEVEN_AND_EIGHT,				// flag for hdwds 7,8 as X,Y
     SEVENTEEN_AND_EIGHTEEN,			// flag for hdwds 17,18 as X,Y
     THIRTYSEVEN_AND_THIRTYEIGHT,		// flag for hdwds 37,38 as X,Y
     EIGHT_AND_SEVEN,				// flag for hdwds 8,7 as X,Y
     EIGHTEEN_AND_SEVENTEEN,			// flag for hdwds 18,17 as X,Y
     THIRTYEIGHT_AND_THIRTYSEVEN,		// flag for hdwds 38,37 as X,Y
     XYHEADERS_UNSPECIFIED = -1};		// flag for unspecified X,Y

  enum
    {PARSE_BY_XHDRWD,				// flag to parse on X hdwd
     PARSE_BY_YHDRWD,				// flag to parse on Y hdwd
     DO_NOT_PARSE,				// flag not to parse
     PARSE_BY_GT_90,				// flag to parse on >=90deg
     PARSE_UNSPECIFIED = -1};			// flag for unspecified parse

  static int numXYHeaders ();			// rtn # of X,Y hdr options

  static int minimumValidXYHeader ();		// rtn min X,Y hdr option

  static int maximumValidXYHeader ();		// rtn max X,Y hdr option

  static int verifyXYHeader			// 0 if not valid X,Y hdr opt
    (int xy_header_words);			//   given flag for X,Y hdwds

  static const char *getXYHeaderString		// rtn X,Y hdwd string descrip
    (int xy_header);				//   given flag for X,Y hdwds

  static const char *getXYHeaderOneWord		// rtn 1-wrd X,Y hdwd string
    (int xy_header);				//   given flag for X,Y hdwds

  static int getXYHeaderWords			// rtn flag for X,Y hdwds
    (char *xy_hdrwrds_one_word);		//   given 1-wrd X,Y hdwd str

  static int getHeaderWordOffset		// rtn offset to hdr wrd
    (int xy_header_words,			//   given flag for X,Y hdwds
     int which_header);				//   given either X or Y hdr f

  static int getHeaderIdent			// rtn header ident
    (int xy_header_words,			//   given flag for X,Y hdwds
     int which_header);				//   given either X or Y hdr f

  static int getYHeaderIdentFromX		// rtn Y-header ident
    (int x_header_ident);			//   given X-header ident

  static int numLineParsers ();			// rtn # of line parse options
 
  static int minimumValidLineParser ();		// rtn min line parser option

  static int maximumValidLineParser ();		// rtn max line parser option

  static int verifyLineParser			// 0 if not valid line parser
    (int line_parser);				//   given flg for line parser

  static const char *getLineParserString	// rtn line parser string desc
    (int line_parser);				//   given flg for line parser

  static const char *getLineParserOneWord	// rtn 1-wrd line parser strng
    (int line_parser);				//   given flg for line parser

  static int getLineParseMethod			// rtn flag for line parser
    (char *line_parser_one_word);		//   given 1-wrd line parser s

};

#endif
