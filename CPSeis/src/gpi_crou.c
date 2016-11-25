/****
!<CPS_v1 type="AUXILIARY_FILE"/>
****/
/*----------------------------- gpi_crou.c -------------------------------*/
/*----------------------------- gpi_crou.c -------------------------------*/
/*----------------------------- gpi_crou.c -------------------------------*/

    /* other files are:  gpi.f90 */

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
!                        C P S  P R I M I T I V E   F I L E
!
! Name       : GPI_CROU
! Category   : miscellaneous
! Written    : 2002-08-09   by: Ed Schmauch
! Revised    : 2005-10-24   by: tom Stoeckley
! Maturity   : production
! Purpose    : Provide CFE with interface to gnuplot.
! Portability: Requires unix, gnuplot, and posix threads.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author      Description
!     ----        ------      -----------
!  3. 2005-10-24  Stoeckley   More fixes to compile with C++.
!  2. 2005-05-31  Stoeckley   Fix to compile with C++.
!  1. 2002-09-11  Ed Schmauch Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>
****/


/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/
/*--------------------------- start of coding ------------------------------*/


char GPI_CROU_IDENT[100] =
"$Id: gpi_crou.c,v 1.3 2005/10/24 11:32:17 Stoeckley prod sps $";

#include "c2f_interface.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/utsname.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <assert.h>

#ifdef NEED_UNDERSCORE
#define gpi_open_c        gpi_open_c_
#define gpi_close_c       gpi_close_c_
#define gpi_close_all_c   gpi_close_all_c_
#define gpi_command_c     gpi_command_c_
#elif defined NEED_CAPITALS
#define gpi_open_c        GPI_OPEN_C
#define gpi_close_c       GPI_CLOSE_C
#define gpi_close_all_c   GPI_CLOSE_ALL_C
#define gpi_command_c     GPI_COMMAND_C
#endif


/*------------------------ start of data ----------------------------*/
/*------------------------ start of data ----------------------------*/
/*------------------------ start of data ----------------------------*/


#define GPI_MEM_INC 5

typedef struct
{
   char      *fifo;
   FILE      *out;
   pthread_t  tid;
   int        fifo_created;
   int        pthread_started;
}  gpi_struct;

static int          gpi_read_thread_success =                 0;
static int          gpi_read_thread_failure =                 1;
static int          gpi_num_structs         =                 0;
static gpi_struct **gpi_array               = (gpi_struct **) 0;


/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/
/*------------------------ start of functions ----------------------------*/

#ifdef __cplusplus
extern "C" {
#endif


static gpi_struct *gpi_create_struct_c(int lun)
{
   gpi_struct    *retval;
   struct utsname host;
   char           buff[BUFSIZ];
   int            nc;

   if (uname(&host) < 0)
   {
      perror("gpi_create_struct_c uname");
      return (gpi_struct *) 0;
   }

   nc = sprintf(buff, "gpi_fifo-%s-%d-%d", host.nodename, (int) getpid(), lun);

   if ((nc < 0) || (nc >= BUFSIZ))
   {
      fprintf(stderr, "gpi_create_struct_c sprintf error\n");
      return (gpi_struct *) 0;
   }

   assert((size_t) nc == strlen(buff));

   if (!(retval = (gpi_struct*)malloc(sizeof(gpi_struct))))
   {
      fprintf(stderr, "gpi_create_struct_c out of memory\n");
      return (gpi_struct *) 0;
   }

   if (!(retval->fifo = (char*)malloc((size_t) (nc + 1))))
   {
      fprintf(stderr, "gpi_create_struct_c out of memory\n");
      free(retval);
      return (gpi_struct *) 0;
   }

   strcpy(retval->fifo, buff);

   retval->out             = (FILE *) 0;
   retval->fifo_created    =          0;
   retval->pthread_started          = 0;

   return retval;
}

static void gpi_delete_struct_c(gpi_struct *gs)
{
   free(gs->fifo);
   free(gs      );
}

static int gpi_new_lun_c()
{
   int          retval;
   gpi_struct **ptr;
   int          i;

   for (retval = 0; retval < gpi_num_structs; retval++)
      if (!gpi_array[retval])
         break;

   if (retval == gpi_num_structs)
   {
      if (!(ptr = (gpi_struct**)realloc(gpi_array,
         sizeof(gpi_struct *) * (size_t) (gpi_num_structs + GPI_MEM_INC))))
      {
         return -1;
      }
      else
      {
         gpi_array = ptr;
      }

      for (i = gpi_num_structs; i < gpi_num_structs + GPI_MEM_INC; i++)
         gpi_array[i] = (gpi_struct *) 0;

      gpi_num_structs += GPI_MEM_INC;
   }

   if (!(gpi_array[retval] = gpi_create_struct_c(retval)))
      retval = -1;

   return retval;
}

static void gpi_free_lun_c(int lun)
{
   assert((lun >= 0) && (lun < gpi_num_structs) && gpi_array[lun]);
   gpi_delete_struct_c(gpi_array[lun]);
   gpi_array[lun] = (gpi_struct *) 0;
}

static void *gpi_read_thread_c(void *arg)
{
   int     in;
   int     nb;
   char    buff[BUFSIZ];

   if ((in  = open(((gpi_struct *) arg)->fifo, O_RDONLY)) < 0)
   {
      perror("gpi_read_thread_c open");
      return &gpi_read_thread_failure;
   }

   for ( ; ; )
   {
      nb = (int) read(in, buff, (size_t) (BUFSIZ-1));
      if (nb < 0)
      {
         perror("gpi_read_thread_c read");
         return &gpi_read_thread_failure;
      }
      else if (nb == 0)
      {
         break;
      }
      else
      {
         buff[nb] = '\0';
         fprintf(stderr, "%s", buff);
      }
   }

   if (close(in) < 0)
   {
      perror("gpi_read_thread_c close");
      return &gpi_read_thread_failure;
   }

   return &gpi_read_thread_success;
}

/*
 * Returns 1 if command ok, 0 if not.
 */
static int gpi_valid_command_c(const char *cmd)
{
   int retval;

   if (strchr(cmd, (int) '\n'))
   {
      fprintf(stderr,
         "gpi_valid_command_c cannot have end-of-record in command\n");
      retval = 0;
   }
   else
   {
      char *loc_cmd;
      char *tok_ptr;

      /*
       * Make local copy of cmd because strtok inserts nulls.
       */
      loc_cmd = (char*)malloc(strlen(cmd) + (size_t) 1);
      if (!loc_cmd)
      {
         fprintf(stderr, "gpi_valid_command_c out of memory\n");
         return 0;
      }

      strcpy(loc_cmd, cmd);

      tok_ptr = strtok(loc_cmd, " \t");
      if (tok_ptr)
      {
         /*
          * Don't let command be quit, gnuplot exiting outside of gpi_close_c
          * will lead to broken pipe.
          */
         static const char *quitter[] =
            { "quit", "qui", "qu", "q", "exit", "exi", "ex" };
         const int num_quitters = (int) (sizeof(quitter) / sizeof(char *));
         int i;

         for (retval = 1, i = 0; retval && (i < num_quitters); i++)
            if (!strcmp(tok_ptr, quitter[i]))
            {
               fprintf(stderr, "gpi_valid_command_c use gpi_close_c to quit\n");
               retval = 0;
            }
      }
      else
      {
         fprintf(stderr, "gpi_valid_command_c blank command\n");
         retval = 0;
      }

      free(loc_cmd);
   }

   return retval;
}

/*
 * Returns 0 if ok, -1 if error.
 * gpi_close_c defined before gpi_open_c because gpi_open_c calls gpi_close_c
 * and we are avoiding prototypes.
 */
INTEGER gpi_close_c(INTEGER *lun_ptr)
{
   int         retval;
   int         lun = (int) *lun_ptr;
   gpi_struct *gs;
   void       *status;

   if ((lun >= 0) && (lun < gpi_num_structs) && gpi_array[lun])
   {
      retval = 0;
      gs = gpi_array[lun];

      if (gs->out && (fprintf(gs->out, "quit\n") == EOF))
      {
         perror("gpi_close_c fprintf");
         retval = -1;
      }

      if (gs->pthread_started)
      {
         if (pthread_join(gs->tid, &status))
         {
            fprintf(stderr, "gpi_close_c pthread_join error\n");
            retval = -1;
         }
         else if (*((int *) status) == gpi_read_thread_failure)
         {
            fprintf(stderr, "gpi_close_c gpi_read_thread status error\n");
            retval = -1;
         }
      }

      if (gs->out && pclose(gs->out))
      {
         perror("gpi_close_c pclose");
         retval = -1;
      }

      if (gs->fifo_created && (unlink(gs->fifo) < 0))
      {
         perror("gpi_close_c unlink");
         retval = -1;
      }

      gpi_free_lun_c(lun);
   }
   else
   {
      fprintf(stderr, "gpi_close_c bad lun\n");
      retval = -1;
   }

   return (INTEGER) retval;
}

/*
 * Returns 0 if ok, -number of close failures if error.
 */
INTEGER gpi_close_all_c()
{
   INTEGER retval;
   INTEGER i;

   for (retval = i = (INTEGER) 0; i < (INTEGER) gpi_num_structs; i++)
      if (gpi_array[(int) i])
         retval += gpi_close_c(&i);

   return retval;
}

/*
 * Returns new logical unit for gnuplot.
 * Returns -1 if error.
 */
INTEGER gpi_open_c()
{
   INTEGER     retval;
   gpi_struct *gs;
   int         nc;
   char        buff[BUFSIZ];

   if ((retval = (INTEGER) gpi_new_lun_c()) != (INTEGER) -1)
   {
      gs = gpi_array[(int) retval];

      if (!access(gs->fifo, F_OK))
         if (unlink(gs->fifo) < 0)
         {
            perror("gpi_open_c unlink");
            gpi_close_c(&retval);
            return (INTEGER) -1;
         }

      if (mkfifo(gs->fifo, 0600) < 0)
      {
         perror("gpi_open_c mkfifo");
         gpi_close_c(&retval);
         return (INTEGER) -1;
      }
      else
      {
         gs->fifo_created = 1;
      }

      nc = sprintf(buff, "gnuplot > %s 2>&1", gs->fifo);
      if ((nc < 0) || (nc >= BUFSIZ))
      {
         fprintf(stderr, "gpi_open_c sprintf error\n");
         gpi_close_c(&retval);
         return (INTEGER) -1;
      }

      if ((gs->out = popen(buff, "w")) == NULL)
      {
         perror("gpi_open_c popen");
         gpi_close_c(&retval);
         return (INTEGER) -1;
      }

      if (setvbuf(gs->out, NULL, _IOLBF, 0) != 0)
      {
         perror("gpi_open_c setvbuf");
         gpi_close_c(&retval);
         return (INTEGER) -1;
      }

      if (pthread_create(&gs->tid, NULL, gpi_read_thread_c, gs))
      {
         fprintf(stderr, "pthread_create error\n");
         gpi_close_c(&retval);
         return (INTEGER) -1;
      }
      else
      {
         gs->pthread_started = 1;
      }
   }

   return retval;
}

/*
 * Returns 0 if ok, -1 if error.
 */
INTEGER gpi_command_c(INTEGER *lun_ptr, const char *cmd)
{
   int retval;
   int lun = (int) *lun_ptr;

   if ((lun >= 0) && (lun < gpi_num_structs) && gpi_array[lun])
   {
      if (!gpi_valid_command_c(cmd))
      {
         retval = -1;
      }
      else if (fprintf(gpi_array[lun]->out, "%s\n", cmd) == EOF)
      {
         perror("gpi_command_c fprintf");
         retval = -1;
      }
      else
      {
         retval =  0;
      }
   }
   else
   {
      fprintf(stderr, "gpi_command_c bad lun\n");
      retval = -1;
   }

   return (INTEGER) retval;
}

/*------------------------ start of test code ----------------------------*/
/*------------------------ start of test code ----------------------------*/
/*------------------------ start of test code ----------------------------*/

/* <TEST_CODE>

#include "c2f_interface.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>

#ifdef NEED_UNDERSCORE
#define gpi_open_c        gpi_open_c_
#define gpi_close_c       gpi_close_c_
#define gpi_close_all_c   gpi_close_all_c_
#define gpi_command_c     gpi_command_c_
#elif defined NEED_CAPITALS
#define gpi_open_c        GPI_OPEN_C
#define gpi_close_c       GPI_CLOSE_C
#define gpi_close_all_c   GPI_CLOSE_ALL_C
#define gpi_command_c     GPI_COMMAND_C
#endif

INTEGER gpi_open_c();
INTEGER gpi_close_c(INTEGER *lun_ptr);
INTEGER gpi_close_all_c();
INTEGER gpi_command_c(INTEGER *lun_ptr, const char *cmd);

int main()
{
   const char *  cmd[] = { "open", "close", "close_all", "command" };
   const int num_cmds  = (int) (sizeof(cmd) / sizeof(char *));
   char      buff[BUFSIZ];
   char     *tokptr;
   INTEGER   Ilun;
   int       ilun;
   char      junk;
   size_t    init_len;
   int       i;

   for ( ; ; )
   {
      if (!fgets(buff, BUFSIZ, stdin))
         break;

      init_len = strlen(buff) - (size_t) 1;
      buff[init_len] = '\0';

      tokptr = strtok(buff, " \t");
      if (!tokptr)
      {
         fprintf(stderr, "?huh?\n");
         continue;
      }

      for (i = 0; i < num_cmds; i++)
         if (!strcmp(tokptr, cmd[i]))
            break;

      if (i == num_cmds)
      {
         fprintf(stderr, "?huh?\n");
         continue;
      }

      switch (i)
      {
         case 0:
            printf("status:  %d\n", (int) gpi_open_c());
            break;
         case 1:
         case 3:
            tokptr = strtok((char *) 0, " \t");
            if (!tokptr)
            {
               fprintf(stderr, "?huh?\n");
            }
            else if (sscanf(tokptr, "%d%c", &ilun, &junk) == 1)
            {
               Ilun = (INTEGER) ilun;
               if (i == 1)
               {
                  printf("status:  %d\n", (int) gpi_close_c(&Ilun));
               }
               else
               {
                  tokptr = strtok((char *) 0, " \t");
                  if (!tokptr)
                  {
                     fprintf(stderr, "?huh?\n");
                  }
                  else
                  {
                     size_t len = strlen(tokptr);
                     if (tokptr - buff + len < init_len)
                        tokptr[len] = ' ';
                     printf("status:  %d\n",
                        (int) gpi_command_c(&Ilun, tokptr));
                  }
               }
            }
            else
            {
               fprintf(stderr, "?huh?\n");
            }
            break;
         case 2:
            printf("status:  %d\n", (int) gpi_close_all_c());
            break;
         default:
            assert(0);
      }
   }

   gpi_close_all_c();

   return 0;
}

</TEST_CODE> */

#ifdef __cplusplus
}
#endif


/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/
/*-------------------------------- end ------------------------------------*/

