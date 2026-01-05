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

#include <stdlib.h>
#include <assert.h>
#include <sys/stat.h>

void * GC_malloc(size_t size_in_bytes);

struct stat * get_file_stat(char * file)
{
  struct stat * fileAttrs = GC_malloc(sizeof(struct stat));
  if (stat(file, fileAttrs) == 0) 
    return fileAttrs;
  return NULL;
}

int compare_time_stamps(struct stat * stat1, struct stat * stat2)
{
  if (stat1 != NULL && stat2 == NULL)
    return 1;
  if (stat1 == NULL)
    return 0;
  assert(stat2 != NULL && stat1 != NULL);
  return stat1->st_mtime >= stat2->st_mtime;
}

