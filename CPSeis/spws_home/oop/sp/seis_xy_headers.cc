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
#include "sp/seis_xy_headers.hh"
#include "cprim.h"
#include <assert.h>

struct XYHeader {
  int xyhdrwds;
  char *xyhdrwds_string;
  char *xyhdrwds_one_word;
};

/* the following was a good idea that RWL had but it had to be abandoned
 * because the underlay stuff was not flexible enough to support rotated
 * coordinates -- when Image is rewritten it needs to handle coordinate
 * transformations in a similar way as GpDataUnit
 */
static XYHeader XYHEADERS[] = {
  { SeisXYHeaders::SEVEN_AND_EIGHT            , " 7 &  8",  "7&8"   },
  { SeisXYHeaders::SEVENTEEN_AND_EIGHTEEN     , "17 & 18",  "17&18" },
  { SeisXYHeaders::THIRTYSEVEN_AND_THIRTYEIGHT, "37 & 38",  "37&38" },
  { SeisXYHeaders::EIGHT_AND_SEVEN            , " 8 &  7",  "8&7"   },
  { SeisXYHeaders::EIGHTEEN_AND_SEVENTEEN     , "18 & 17",  "18&17" },
  { SeisXYHeaders::THIRTYEIGHT_AND_THIRTYSEVEN, "38 & 37",  "38&37" },
};

static const int NUM_XYHEADERS = sizeof XYHEADERS / sizeof (XYHeader);
int SeisXYHeaders::numXYHeaders         () { return NUM_XYHEADERS; }
int SeisXYHeaders::minimumValidXYHeader () { return 0; }    
int SeisXYHeaders::maximumValidXYHeader () { return numXYHeaders() - 1; }
int SeisXYHeaders::verifyXYHeader       (int xy_header_words)
{
  int retval;

  switch (xy_header_words) {
    case SEVEN_AND_EIGHT:
    case SEVENTEEN_AND_EIGHTEEN:
    case THIRTYSEVEN_AND_THIRTYEIGHT:
    case EIGHT_AND_SEVEN:
    case EIGHTEEN_AND_SEVENTEEN:
    case THIRTYEIGHT_AND_THIRTYSEVEN:
      retval = TRUE;
      break;
    default:
      retval = FALSE;
      break;
  };
  return retval;
}

const char *SeisXYHeaders::getXYHeaderString (int xy_header)
{ return XYHEADERS[xy_header].xyhdrwds_string; }

const char *SeisXYHeaders::getXYHeaderOneWord (int xy_header)
{ return XYHEADERS[xy_header].xyhdrwds_one_word; }

int SeisXYHeaders::getXYHeaderWords (char *xyhdrwds_one_word)
{
  for (int k2 = 0; k2 < numXYHeaders(); k2++) {
    if (!strcmp(xyhdrwds_one_word,XYHEADERS[k2].xyhdrwds_one_word)) {
      return XYHEADERS[k2].xyhdrwds;
    }
  }
  return XYHEADERS_UNSPECIFIED;
}

int SeisXYHeaders::getHeaderWordOffset (int xy_header_words,
  int which_header)
{
  int retval;

  switch (which_header) {
    case XHEADER:
      {
        switch (xy_header_words) {
          case SEVEN_AND_EIGHT:
            retval = HEADER_SEVEN;
            break;
          case SEVENTEEN_AND_EIGHTEEN:
            retval = HEADER_SEVENTEEN;
            break;
          case THIRTYSEVEN_AND_THIRTYEIGHT:
            retval = HEADER_THIRTYSEVEN;
            break;
          case EIGHT_AND_SEVEN:
            retval = HEADER_EIGHT;
            break;
          case EIGHTEEN_AND_SEVENTEEN:
            retval = HEADER_EIGHTEEN;
            break;
          case THIRTYEIGHT_AND_THIRTYSEVEN:
            retval = HEADER_THIRTYEIGHT;
            break;
          default:
            assert (0);
            break;
        };
      }
      break;
    case YHEADER:
      {
        switch (xy_header_words) {
          case SEVEN_AND_EIGHT:
            retval = HEADER_EIGHT;
            break;
          case SEVENTEEN_AND_EIGHTEEN:
            retval = HEADER_EIGHTEEN;
            break;
          case THIRTYSEVEN_AND_THIRTYEIGHT:
            retval = HEADER_THIRTYEIGHT;
            break;
          case EIGHT_AND_SEVEN:
            retval = HEADER_SEVEN;
            break;
          case EIGHTEEN_AND_SEVENTEEN:
            retval = HEADER_SEVENTEEN;
            break;
          case THIRTYEIGHT_AND_THIRTYSEVEN:
            retval = HEADER_THIRTYSEVEN;
            break;
          default:
            assert (0);
            break;
        };
      }
      break;
    default:
      assert (0);
      break;
  };
  return retval;
}

int SeisXYHeaders::getHeaderIdent (int xy_header_words,
  int which_header)
{
  return getHeaderWordOffset(xy_header_words,which_header) + 1;
}

// assume that the header values are always (1,2)(7,8), or (17,18), or (37,38)
//   even though the (x,y) can be interchanged!
//Headers 1 and 2 are special case for time slices
int SeisXYHeaders::getYHeaderIdentFromX (int x_header_ident)
{
  int retval;

  switch (x_header_ident) {
    case 1:
    case 7:
    case 17: 
    case 37:
      retval = x_header_ident + 1;
      break;
    case 2:
    case 8:
    case 18:
    case 38:
      retval = x_header_ident - 1;
      break;
    default:
      assert (0);
      break;
  };
  return retval;
}

struct LineParser {
  int line_parser;
  char *line_parser_string;
  char *line_parser_one_word;
};

static LineParser LINEPARSERS[] = {
  { SeisXYHeaders::PARSE_BY_XHDRWD  , "X header word",  "XHeaderWord" },
  { SeisXYHeaders::PARSE_BY_YHDRWD  , "Y header word",  "YHeaderWord" },
  { SeisXYHeaders::DO_NOT_PARSE     , "Do not parse ",  "DoNotParse"  },
  { SeisXYHeaders::PARSE_BY_GT_90   , "Turn > 90 deg",  "Turn>90Deg"  },
};

static const int NUM_LINEPARSERS = sizeof LINEPARSERS / sizeof (LineParser);
int SeisXYHeaders::numLineParsers         () { return NUM_LINEPARSERS; }
int SeisXYHeaders::minimumValidLineParser () { return 0; }    
int SeisXYHeaders::maximumValidLineParser () { return numLineParsers()-1;}

int SeisXYHeaders::verifyLineParser       (int line_parser)
{
  int retval;

  switch (line_parser) {
    case PARSE_BY_XHDRWD:
    case PARSE_BY_YHDRWD:
    case DO_NOT_PARSE:
    case PARSE_BY_GT_90:
      retval = TRUE;
      break;
    default:
      retval = FALSE;
      break;
  };
  return retval;
}

const char *SeisXYHeaders::getLineParserString (int line_parser)
{ return LINEPARSERS[line_parser].line_parser_string; }

const char *SeisXYHeaders::getLineParserOneWord (int line_parser)
{ return LINEPARSERS[line_parser].line_parser_one_word; }

int SeisXYHeaders::getLineParseMethod (char *line_parser_one_word)
{
  for (int k2 = 0; k2 < numLineParsers(); k2++) {
    if (!strcmp(line_parser_one_word,LINEPARSERS[k2].line_parser_one_word)) {
      return LINEPARSERS[k2].line_parser;
    }
  }
  return PARSE_UNSPECIFIED;
}
