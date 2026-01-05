/*
 * SAL 3.1, Copyright (C) 2006, 2011, SRI International.  All Rights Reserved.
 *
 * SAL is free software; you can redistribute it and/or 
 * modify it under the terms of the GNU General Public License 
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
 * GNU General Public License for more details. 
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software 
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. 
 *
 */

#include <time.h>

#include "bigloo.h"

typedef struct clock_list * clock_list_ptr;

struct clock_list {
  clock_t runtime_start;
  clock_list_ptr next;
};

clock_list_ptr ClockList = NULL;

#ifdef HAS_SYS_TIMES

#include <sys/times.h>
#include <unistd.h>
#include <errno.h>

/*
 * BD: When the 'times' function is used, the conversion from clock_t to 
 * seconds is system dependent.
 * 
 * - on Linux (don't know since which kernel version): 
 *   CLK_TCK is obsolete (not defined in <time.h>)
 *   CLOCKS_PER_SEC is useless (always defined to be 1000000)
 *   we need to get the number of clock ticks per seccond using sysconf(_SC_CLK_TCK)
 *
 * - on Darwin
 *   CLK_TCK is defined and can be used 
 *   CLOCKS_PER_SEC is equal to CLK_TCK
 *   sysconf(_SC_CLK_TCK) returns CLK_TCK
 *
 * - on Solaris
 *   CLOCKS_PER_SEC can be used
 *   sysconf(_SC_CLK_TCK) is defined too
 *
 * - on Cygwin:
 *   sys/times.h doesn't seem to be present
 *   sysconf(_SC_CLC_TCK ) is available?
 *   so we can use clock and CLOCKS_PER_SEC
 *   
 */

static struct tms TimeInfo;

void initialize_runtime()
{
  clock_list_ptr new_node = (clock_list_ptr) GC_malloc(sizeof(struct clock_list));
  times(&TimeInfo);
  new_node->runtime_start = TimeInfo.tms_utime + TimeInfo.tms_stime + TimeInfo.tms_cutime + TimeInfo.tms_cstime; /* collects children processes time */
  new_node->next = ClockList;
  ClockList = new_node;
}

/*
 * BD: attempt to make this more reliable.
 */
double collect_elapsed()
{
  clock_t end;
  int saved_errno;
  double result;
  long ticks_per_sec;

  result = -1.0;            // default value on error

  errno = 0;
  (void) times(&TimeInfo);  // times returns -1 on error (but that's not reliable)
  saved_errno = errno;

  ticks_per_sec = sysconf(_SC_CLK_TCK);  // ticks_per_sec < 0 on error

  if (saved_errno == 0 && ticks_per_sec > 0) {
    end = TimeInfo.tms_utime + TimeInfo.tms_stime + TimeInfo.tms_cutime + TimeInfo.tms_cstime; 
    result = ((double) (end - ClockList->runtime_start)) / ticks_per_sec;
  }

  return result;
}

double collect_runtime()
{
  double result = collect_elapsed();
  ClockList = ClockList->next;
  return result;
}

#else

/* sys/times.h is not available... using less precise functions... */

void initialize_runtime()
{
  clock_list_ptr new_node = (clock_list_ptr) GC_malloc(sizeof(struct clock_list));
  new_node->runtime_start = clock();
  new_node->next = ClockList;
  ClockList = new_node;
}

double collect_elapsed()
{
  clock_t end; 
  double result;
  end = clock();
  result = ((double) (end - ClockList->runtime_start)) / CLOCKS_PER_SEC;
  return result;
}

double collect_runtime()
{
  double result = collect_elapsed();
  ClockList = ClockList->next;
  return result;
}

#endif



/*
 * BD: also added these functions to extract meaniningful information
 * from the code returns by bigloo's (system ....) calls.
 *
 * Example, when we call (system "lingeling" ...) we want to
 * check the exit code from lingeling. But what (system ..)
 * returns is the integer status returned by the OS system 
 * function.
 *
 * This is a process status in the same format as used by wait.
 * To get the process's exit code from this, we need to 
 * use macros defined in <sys/wait.h>
 */

#include <sys/wait.h>

// what caused the process to exit
extern int wifexited(int status) {
  return WIFEXITED(status);
}

extern int wifsignaled(int status) {
  return WIFSIGNALED(status);
}

extern int wifstopped(int status) {
  return WIFSTOPPED(status);
}


// process exit code
// valid only if wifexited(status) is true)
extern int wexitstatus(int status) {
  return WEXITSTATUS(status);
}

extern int wtermsig(int status) {
  return WTERMSIG(status);
}

extern int wcoredump(int status) {
#ifdef WCOREDUMP
  return WCOREDUMP(status);
#else
  return 0;
#endif
}

extern int wstopsig(int status) {
  return WSTOPSIG(status);
}
