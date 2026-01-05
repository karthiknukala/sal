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

#ifndef STATE_ENTRY_H
#define STATE_ENTRY_H

#define WORD_NUM_BITS (sizeof(unsigned) * 8)

int SE_init();
int SE_eq(unsigned * entry1, unsigned * entry2, unsigned num_words);
unsigned SE_hash(unsigned * entry, unsigned num_words);
unsigned SE_hash_increment(unsigned * entry, unsigned num_words);
#define SE_alloc(size) ((unsigned *) GC_malloc_atomic(size))

#endif /* STATE_ENTRY_H */
