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
/*=========================================================================*/
/*== Stand alone program to generate a csv compatible file from a        ==*/
/*== trace file. This program requires a legitimate regular grid         ==*/
/*== trace file name as input and returns a status of 1 if successful    ==*/
/*== Author Michael L. Sherrill 04/16/2002.                              ==*/
/*=========================================================================*/


#include "cube.h"

#define ERROR_PARSING -1

#define HELP "\n\nUsage is input filename and optional crossline header and \n\
inline header (which default to 7 and 8). \n\nEXAMPLE: csvtrot filename 37 38\n\n"




//===== Print to the screen the percent done in increments of 10% =========
static int status_function (void *obj, float percent_done)
{
  int percent;
  static int last_percent = 100;

  percent = (int)(percent_done * 100);

  if(percent && percent % 10 == 0 && percent != last_percent)
    {
      if(percent == 100)
        printf("Stand by... wrapping up\n");
      else
        printf("Writing csv file... percent completed = %d\n",percent);
      last_percent = percent;
    }

  return 0;
}

//================================= Main ===================================

int main(int argc, char *argv[])
{
  char filename[512];
  int ok = 0, error;
  int number_successful;
  int junk = 1;
  char *firstchar;
  int crossline_header = 7, inline_header = 8;

  // Not enough info passed on the command line
  if(argc < 2)
    {
      printf(HELP);
      return ERROR_PARSING;
    }

  // Get the file name
  number_successful = sscanf(argv[1], "%s", filename);
  if(!number_successful) 
    {
      printf("Error parsing the filename\n");
      printf(HELP);
      return ERROR_PARSING;
    }

  //See if user has specified headers
  if(argc > 2)
    {
      if(argc != 4)
        {
          printf(HELP);
          return ERROR_PARSING;
        }

      number_successful = sscanf(argv[2], "%d", &crossline_header);      
      if(!number_successful) 
        {
          printf("Error parsing the crossline header\n");
          printf(HELP);
          return ERROR_PARSING;
        }

      number_successful = sscanf(argv[3], "%d", &inline_header);      
      if(!number_successful) 
        {
          printf("Error parsing the inline header\n");
          printf(HELP);
          return ERROR_PARSING;
        }
    }

  // A period as the first character is not yet supported
  firstchar = filename;
  if(*firstchar == '.')
    {
      printf("Sorry the period notation for the path is not yet supported.\n");
      return ERROR_PARSING;
    }  


  // Ok to continue processing
  printf("Attempting to create the CSV file from %s\n",filename);
  printf("Please wait, allocating disk space...\n");

  error = cube_trcio_create_file(filename, status_function, &junk,
/*
                                 crossline_header, inline_header, 1);
*/
                                 crossline_header, inline_header,1);
  if(error)
    {
    printf("Error creating CSV file from input file %s\n",filename);
    printf(HELP);
    return error;
    }


  printf("OK. Finished creating csv file from %s\n",filename);

  return ok;
}
