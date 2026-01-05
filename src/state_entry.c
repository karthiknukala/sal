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

#include<stdlib.h>

unsigned g_seed_hash;
unsigned g_seed_hash_inc;

int SE_init() {
  g_seed_hash = rand();
  g_seed_hash_inc = rand();
}

int SE_eq(unsigned * entry1, unsigned * entry2, unsigned num_words) {
  unsigned i;
  for(i = 0; i < num_words; i++)
    if (entry1[i] != entry2[i])
      return 0;
  return 1;
}

unsigned SE_hash(unsigned * entry, unsigned num_words) {
  unsigned i;
  unsigned r = 0;
  // r = jenkins_hash(entry, sizeof(unsigned)*ext_offset, g_seed_hash);
  // jenkins hash is more precise and useful for supertrace methods...
  for(i = 0; i < num_words; i++)
    r = r * 3 + entry[i];
  return r;
}

unsigned SE_hash_increment(unsigned * entry, unsigned num_words) {
  int i;
  unsigned r = 0;
  // r = jenkins_hash(entry, sizeof(unsigned)*ext_offset, g_seed_hash_inc);
  for(i = num_words - 1; i >= 0; i--)
    r = r * 5 + entry[i];
  return r;
}

