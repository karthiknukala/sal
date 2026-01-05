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

#include<stdio.h>
#include<stdlib.h>
#include<assert.h>
#include<string.h>
#include"state_entry.h"
#include"state_entry_channel.h"

void * GC_malloc(size_t size_in_bytes);
void * GC_malloc_atomic(size_t size_in_bytes);
void c_int_verbose_message(int lvl, char * msg, int);

struct STDL_state_to_do_list {
  unsigned * m_start;
  unsigned * m_finish;
  unsigned * m_end_of_storage;
  unsigned   m_entry_size;
  unsigned   m_words_per_entry;
  unsigned   m_next_idx;
  unsigned   m_first_idx;
  unsigned   m_num_base_words;
  SEC_state_entry_channel_ptr m_input_channel;
  SEC_state_entry_channel_ptr m_output_channel;
};

typedef struct STDL_state_to_do_list * STDL_state_to_do_list_ptr;

#define INITIAL_CAPACITY 65536

#define WORD_NUM_BITS (sizeof(unsigned) * 8)

STDL_state_to_do_list_ptr STDL_make(unsigned aux_num_bits, int store_depth) {
  STDL_state_to_do_list_ptr result;
  unsigned words_per_entry;
  unsigned entry_size;
  unsigned num_bits = 
    2 * WORD_NUM_BITS + // two extra words to store state_idx and parent_idx
    (store_depth ? WORD_NUM_BITS : 0) +  // one extra word if we need to store the depth
    aux_num_bits; 
  result                           = (STDL_state_to_do_list_ptr) GC_malloc(sizeof(struct STDL_state_to_do_list));
  result->m_input_channel          = SEC_make(num_bits);
  result->m_output_channel         = SEC_make(num_bits);
  entry_size                       = SEC_entry_size(result->m_input_channel);
  words_per_entry                  = entry_size / sizeof(unsigned);
  result->m_start                  = (unsigned *) GC_malloc_atomic(INITIAL_CAPACITY * entry_size);
  memset(result->m_start, 0, INITIAL_CAPACITY * entry_size);
  result->m_finish                 = result->m_start;
  result->m_end_of_storage         = result->m_start + (INITIAL_CAPACITY * words_per_entry);
  result->m_entry_size             = entry_size;
  result->m_words_per_entry        = words_per_entry;
  result->m_num_base_words         = store_depth ? 3 : 2;
  result->m_next_idx               = 1; // entry 0 is not used
  result->m_first_idx              = 1; // entry 0 is not used
  result->m_finish                += words_per_entry; // entry 0 is not used
  return result;
}

unsigned  STDL_size(STDL_state_to_do_list_ptr alist) {
  return alist->m_next_idx - alist->m_first_idx;
}

int STDL_empty(STDL_state_to_do_list_ptr alist) {
  return (alist->m_next_idx - alist->m_first_idx) == 0;
}

void expand_list(STDL_state_to_do_list_ptr alist) {
  unsigned curr_num_words = (alist->m_end_of_storage - alist->m_start);
  unsigned curr_size = curr_num_words * sizeof(unsigned);
  unsigned new_size = curr_size * 2;
  unsigned delta = alist->m_finish - alist->m_start;
  // printf("state: %d, finish: %d, end: %d\n", alist->m_start, alist->m_finish, alist->m_end_of_storage);
  unsigned * new_start = (unsigned *) GC_malloc_atomic(new_size);
  c_int_verbose_message(4, "    expanding to do list, new size: ~a bytes...", new_size);
  memcpy(new_start, alist->m_start, curr_size);
  memset(new_start + curr_num_words, 0, curr_size);
  assert(memcmp(new_start, alist->m_start, curr_size) == 0);
  alist->m_start = new_start;
  alist->m_finish = new_start + delta;
  alist->m_end_of_storage = new_start + (new_size / sizeof(unsigned));
  // printf("new state: %d, finish: %d, end: %d\n", alist->m_start, alist->m_finish, alist->m_end_of_storage);
}

unsigned * STDL_get_entry(STDL_state_to_do_list_ptr alist, unsigned idx) {
  unsigned words_per_entry = alist->m_words_per_entry;
  return alist->m_start + (idx * words_per_entry);
}

SEC_state_entry_channel_ptr STDL_reset_input_channel(STDL_state_to_do_list_ptr alist, unsigned state_idx, 
                                                     unsigned parent_idx) {
  SEC_state_entry_channel_ptr channel = alist->m_input_channel;
  SEC_input_reset(channel);
  SEC_add_aligned_32_bit(channel, state_idx);
  SEC_add_aligned_32_bit(channel, parent_idx);
  return channel;
}

SEC_state_entry_channel_ptr STDL_reset_input_channel_with_depth(STDL_state_to_do_list_ptr alist, unsigned state_idx, 
                                                                unsigned parent_idx, unsigned depth_idx) {
  SEC_state_entry_channel_ptr channel = alist->m_input_channel;
  SEC_input_reset(channel);
  SEC_add_aligned_32_bit(channel, state_idx);
  SEC_add_aligned_32_bit(channel, parent_idx);
  SEC_add_aligned_32_bit(channel, depth_idx);
  return channel;
}


unsigned STDL_get_state_idx(STDL_state_to_do_list_ptr alist, unsigned entry_idx) {
  unsigned * entry;
  assert(entry_idx > 0);
  entry = STDL_get_entry(alist, entry_idx);
  return entry[0];
}

void STDL_set_state_idx(STDL_state_to_do_list_ptr alist, unsigned entry_idx, unsigned state_idx) {
  unsigned * entry;
  assert(entry_idx > 0);
  assert(entry_idx < alist->m_next_idx);
  entry = STDL_get_entry(alist, entry_idx);
  entry[0] = state_idx;
}

unsigned STDL_get_parent_idx(STDL_state_to_do_list_ptr alist, unsigned entry_idx) {
  unsigned * entry;
  assert(entry_idx > 0);
  entry = STDL_get_entry(alist, entry_idx);
  return entry[1];
}

unsigned STDL_get_depth(STDL_state_to_do_list_ptr alist, unsigned entry_idx) {
  unsigned * entry;
  assert(entry_idx > 0);
  entry = STDL_get_entry(alist, entry_idx);
  return entry[2];
}

unsigned STDL_get_next_idx(STDL_state_to_do_list_ptr alist) {
  return alist->m_next_idx;
}

SEC_state_entry_channel_ptr STDL_reset_output_channel(STDL_state_to_do_list_ptr alist, unsigned entry_idx) {
  SEC_state_entry_channel_ptr channel = alist->m_output_channel;
  channel->m_entry = STDL_get_entry(alist, entry_idx);
  SEC_output_reset(channel);
  SEC_consume_32_bit_words(channel, alist->m_num_base_words); // consume state_idx and parent_idx
  return channel;
}

unsigned STDL_insert(STDL_state_to_do_list_ptr alist, SEC_state_entry_channel_ptr channel) {
  unsigned result;
  unsigned * entry;
  if (alist->m_finish + alist->m_words_per_entry > alist->m_end_of_storage) 
    expand_list(alist);
  result = alist->m_next_idx;
  entry = STDL_get_entry(alist, result);
  // printf("entry[0]: %d\n", SEC_entry(channel)[0]);
  memcpy(entry, SEC_entry(channel), alist->m_entry_size);
  // printf("new_entry[0]: %d\n", entry[0]);
  alist->m_finish += alist->m_words_per_entry;
  alist->m_next_idx++;
  return result;
}

unsigned STDL_remove_top(STDL_state_to_do_list_ptr alist) {
  assert(!STDL_empty(alist));
  alist->m_next_idx--;
  alist->m_finish -= alist->m_words_per_entry;
  // printf("removed[0]: %d\n", entry[0]);
  return alist->m_next_idx;
}

unsigned STDL_remove_front(STDL_state_to_do_list_ptr alist) {
  unsigned result = alist->m_first_idx;
  assert(!STDL_empty(alist));
  alist->m_first_idx++;
  return result;
}
