/****
!<CPS_v1 type=PRIMITIVE"/>
****/
/*---------------------------- readcard.c ----------------------------------*/
/*---------------------------- readcard.c ----------------------------------*/
/*---------------------------- readcard.c ----------------------------------*/

                    /* other files are:  readcard.h */


/****
!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E        
!
! Name       : readcard
! Category   : io
! Written    : 1994-12-28   by: Tom Stoeckley
! Revised    : 2001-02-01   by: Tom Stoeckley
! Maturity   : production   2001-05-14
! Purpose    : read a card image from a text file.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive returns a string which is the image of one or more cards
! read from a text file.  Each card is one line terminated by \n.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
! To read one card (one line) from a text file:
!
!             char *readcard (FILE *stream)
!
! The returned string is terminated by \0.  The \n character at the
! end of the card image is removed from the returned string.
!
! The returned pointer points to a static area which should not
! be modified or freed.  This string should be copied to wherever
! it is needed before the next call to this routine, because the
! same static area is used each time.
!
! Returns a pointer to a blank card (length 0) if a blank line
! is encountered.
!
! Returns NULL for the following reasons:
!   (1) if an EOF was encountered.
!   (2) if a read error occurred.
!   (3) if the line which was read begins with a \0 character.
!   (4) if the line contains any embedded \0 characters.
!   (5) if the line was too long to fit into the internal buffer.
! See the following information to learn details.
!
!-------------------------------------------------------------------------------
! To read several cards (several lines) from a text file:
!
!          char *readcard_several (FILE *stream, int num_lines)
!
! num_lines = number of card images to read and concatenate.
!
! The individual card images are concatenated internally.  The result
! is as if the separate card images were actually one long card image.
!
! The returned string is terminated by \0.  The \n character at the
! end of the last card image is removed from the returned string.
! The \n at the end of each preceding card image is replaced by the
! blank character.
!
! The returned pointer points to a static area which should not
! be modified or freed.  This string should be copied to wherever
! it is needed before the next call to this routine, because the
! same static area is used each time.
!
! Returns a pointer to a blank card (length 0) if any blank lines
! are encountered.
!
! Returns NULL for the following reasons:
!   (1) if an EOF was encountered before all lines are read.
!   (2) if a read error occurred before all lines are read.
!   (3) if any line which was read begins with a \0 character.
!   (4) if any line contains any embedded \0 characters.
!   (5) if any line was too long to fit into the internal buffer.
!   (6) if the sum of all lines was too long to fit into the internal buffer.
! See the following information to learn details.
!
!-------------------------------------------------------------------------------
! To get information about the card(s) just read by one of the above functions:
!
! Include the following header files:
!
!                #include <stdio.h>
!                #include <errno.h>   // defines integer errno.
!                #include <string.h>
!
! Call the following functions in the standard C library:
!
!                 int   feof     (FILE *stream);
!                 int   ferror   (FILE *stream);
!           const char *strerror (errno);
!                 int   strlen   (const char *card);
!
! feof     returns TRUE if an EOF was encountered.
! ferror   returns TRUE if a read error occurred.
! errno    contains an implementation-defined error value.
! strerror returns pointer to a string describing the error.
! strlen   returns length of card if read was successful.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  6. 2001-05-14  Stoeckley  Replace this old version with newer version
!                             which has been used in ~spws for several years.
!                             This old version, which contains a serious bug,
!                             had been replaced in ~spws several years ago but
!                             somehow was mistakingly found and converted
!                             to the new CPS in revision 5.  Up until this
!                             revision 6 date, the version being used in ~sps
!                             lacked revisions 2,3,4.  Recently this old
!                             version has also been used in VA and other ~spws
!                             programs when we started linking the ~spws
!                             programs to testlib a few months ago.
!                            Also change old function names as follows:
!                             from read_card  to readcard;
!                             from read_cards to readcard_several.
!  5. 1999-09-10  O'Brien    Added documentation during CPS conversion.
!                             (Note added 2001-01-31: An old version lacking
!                             revisions 2,3,4 below was mistakenly converted
!                             at this point.)
!  4. 1998-08-21  Stoeckley  Add function read_cards.
!  3. 1995-11-14  Stoeckley  Replace assert (when n == 0) with error return.
!                             (See comments within the function.)
!                             Also add additional documentation.
!  2. 1995-09-18  Stoeckley  Replace fscanf with fgets (see progamming notes).
!  1. 1994-12-12  Stoeckley  Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! The original version of this primitive (before history revision 2) looked
! like this:
!
!       char *read_card(FILE *stream)
!       {
!         static char card[200];
!         int e;
!    
!         strcpy(card, "$$junk$$");               / * for debug purposes * /
!         e = fscanf(stream, "%[^\n]%*c", card);
!         if(e == 0)
!              {
!              e = fscanf(stream, "%*c");
!              card[0] = '\0';
!              }
!         if(e == EOF) return NULL;
!         return card;
!       }
!
! This original version was replaced in history revision 2 because fscanf
! could read in a line longer than the card buffer.
!    fgets allows protection against this.
!    fgets puts at most BUFFER_SIZE-1 characters into the
!       buffer (including \n if there is room), followed by \0.
!
!-------------------------------------------------------------------------------
!</programming_doc>
****/


/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/
/*-------------------------- start of module ------------------------------*/


char READ_CARD_IDENT[100] =
"$Id: readcard.c,v 1.6 2001/05/09 16:38:43 sps prod sps $";


#include "readcard.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>


#define BUFFER_SIZE 1000


/*----------------------------- readcard ----------------------------------*/
/*----------------------------- readcard ----------------------------------*/
/*----------------------------- readcard ----------------------------------*/


char *readcard(FILE *stream)
{
  static char card[BUFFER_SIZE];
  char *e;
  int n;

  e = fgets(card, BUFFER_SIZE, stream);
  if(e == NULL)
      {
        /**
         **  EOF or error occurred.
         **/
      return NULL;
      }
  assert(e == card);
  n = strlen(card);
  if(n == 0)
      {
        /**
         **  Normally, fgets should never return a zero-length string,
         **  because it will add a \n unless the string fills the buffer.
         **  However, if the file is not a text file, it is possible
         **  that the first character read from the file might be a \0.
         **  This happens on the HP when the file is a directory!
         **/
      return NULL;
      }
  else if(card[n-1] == '\n')
      {
      card[n-1] = '\0';     /* get rid of \n character at end of string */
      }
  else
      {
        /**
         **  If n == BUFFER_SIZE-1, this means that the line is too
         **  long to fit into the buffer, and there was not room to
         **  include the \n at the end of the line.  We are considering
         **  this to be an error because it might mean that the file is
         **  not a text file.  We could instead have let the user
         **  get the partial line.  In that case, the next call to
         **  this routine would get the rest of the line (or at least
         **  some of the rest of the line).
         **
         **  Otherwise, this would mean that the file for some reason
         **  contains a \0 in the line just read in.  In this case,
         **  the file would not be a valid text file.
         **/
      return NULL;
      }
  return card;
}


/*-------------------------- readcard several ------------------------------*/
/*-------------------------- readcard several ------------------------------*/
/*-------------------------- readcard several ------------------------------*/


char *readcard_several(FILE *stream, int num_lines)
{
  static char largecard[BUFFER_SIZE];
  int i, totlen = 0;
  assert(num_lines >= 1);
  for(i = 0; i < num_lines; i++)
      {
      char *e = readcard(stream);
      if(e == NULL) return NULL;
      if(i == 0)
          {
          totlen = strlen(e);
          strcpy(largecard, e);
          }
      else
          {
          totlen += 1 + strlen(e);
          if(totlen >= BUFFER_SIZE) return NULL;
          strcat(largecard, " ");
          strcat(largecard, e);
          }
      }
  return largecard;
}


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

