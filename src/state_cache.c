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
#include<stdio.h>
#include<assert.h>
#include<string.h>

#include"state_entry.h"
#include"state_entry_channel.h"

void * GC_malloc(size_t size_in_bytes);
void * GC_malloc_atomic(size_t size_in_bytes);
void c_simple_verbose_message(int lvl, char * msg);
void c_int_verbose_message(int lvl, char * msg, int);
void sc_idx_notification_handler(int * idx_mapping, int size);

#define OA_NIL -1

unsigned g_load_balance = 70;
unsigned g_initial_cache_capacity = 65536; /* 64k states */
unsigned g_num_collisions = 0;

void SC_set_load_balance(unsigned new_val) {
  assert(new_val > 0 & new_val <= 100);
  g_load_balance = new_val;
}

void SC_set_initial_cache_capacity(unsigned new_val) {
  assert(new_val > 0);
  g_initial_cache_capacity = new_val;
}

unsigned SC_get_num_colisions() {
  return g_num_collisions;
}

struct SC_state_cache {
  unsigned * m_start;
  unsigned * m_finish;
  unsigned   m_num_used_entries;
  unsigned   m_entry_capacity;
  unsigned   m_words_per_entry;
  unsigned   m_entry_size;
  int        m_twin_cache;
  SEC_state_entry_channel_ptr m_input_channel;
  SEC_state_entry_channel_ptr m_output_channel;
};

typedef struct SC_state_cache * SC_state_cache_ptr;

#define WORD_NUM_BITS (sizeof(unsigned) * 8)

SC_state_cache_ptr SC_make_core(unsigned num_bits, int twin_cache) {
  unsigned num_entries;
  unsigned entry_size;
  unsigned words_per_entry;
  SC_state_cache_ptr result;
  num_bits                     = num_bits + 1; // use an extra bit to sign whether the entry is available or not.
  if (twin_cache)
    num_bits                   = num_bits + 2; // two extra bits to store information of in which caches the state is stored.
  result                       = (SC_state_cache_ptr) GC_malloc(sizeof(struct SC_state_cache));
  result->m_input_channel      = SEC_make(num_bits);
  result->m_output_channel     = SEC_make(num_bits);
  num_entries                  = g_initial_cache_capacity;
  entry_size                   = SEC_entry_size(result->m_input_channel);
  words_per_entry              = entry_size / sizeof(unsigned);
  result->m_start              = SE_alloc(num_entries * entry_size);
  memset(result->m_start, 0, num_entries * entry_size);
  result->m_finish             = result->m_start + (num_entries * words_per_entry);
  result->m_num_used_entries   = 0;
  result->m_entry_capacity     = g_initial_cache_capacity;
  result->m_words_per_entry    = words_per_entry;
  result->m_entry_size         = entry_size;
  result->m_twin_cache         = twin_cache;
  return result;
}

SC_state_cache_ptr SC_make(unsigned num_bits) {
  return SC_make_core(num_bits, 0);
}

/*
  A twin cache is an efficient representation 
  for a pair of caches. Instead of having
  two caches, each element has extra two bits
  that indicates when:
  - it is in the first cache
  - it is in the second cache
  - it is in both caches
*/
SC_state_cache_ptr SC_make_twin(unsigned num_bits) {
  return SC_make_core(num_bits, 1);
}

SEC_state_entry_channel_ptr SC_reset_input_channel(SC_state_cache_ptr cache) {
  SEC_input_reset(cache->m_input_channel);
  SEC_add_bit(cache->m_input_channel, 1); // mark it as used
  return cache->m_input_channel;
}

unsigned * SC_get_entry(SC_state_cache_ptr cache, unsigned idx) {
  unsigned words_per_entry = cache->m_words_per_entry;
  return cache->m_start + (idx * words_per_entry);
}

SEC_state_entry_channel_ptr SC_reset_output_channel(SC_state_cache_ptr cache, unsigned idx) {
  assert(!SC_is_nil_entry(SC_get_entry(cache, idx)));
  cache->m_output_channel->m_entry = SC_get_entry(cache, idx);
  SEC_output_reset(cache->m_output_channel);
  SEC_read_bit(cache->m_output_channel); // consume mark bit
  return cache->m_output_channel;
}

int SC_get_entry_bit(unsigned * entry, int pos)
{
  unsigned idx = pos / WORD_NUM_BITS;                           
  unsigned offset = pos % WORD_NUM_BITS;                          
  unsigned int mask = 1 << offset;              
  return (entry[idx] & mask) > 0;
}
  
int SC_is_nil_entry(unsigned * entry) {
  return (entry[0] & 1) == 0;
}

int SC_insert_core(SC_state_cache_ptr cache, unsigned * new_entry, unsigned m) {
/*  printf("inserting: "); SC_print_entry(cache, new_entry); */
  unsigned words_per_entry = cache->m_words_per_entry; 
  unsigned * start         = cache->m_start;
  unsigned hash_base       = SE_hash(new_entry, words_per_entry) % m;
  unsigned j               = hash_base;
  unsigned i;
  unsigned inc = 1;
  assert(!SC_is_nil_entry(new_entry));

  for (i = 0; i < m; i++) {
    unsigned * curr_entry = SC_get_entry(cache, j);
    assert(curr_entry >= cache->m_start && curr_entry < cache->m_finish);
    if (SC_is_nil_entry(curr_entry)) {
      memcpy(curr_entry, new_entry, cache->m_entry_size);
      return j;
    }
    else {
      if (SE_eq(curr_entry, new_entry, words_per_entry))
        return -1; // entry is already in the cache
      else
        g_num_collisions ++;
    }
    j = (j + inc) % m;
  }
  assert(0); // unreachable
  return -1;
}

void SC_expand_table(SC_state_cache_ptr cache) {
  unsigned curr_capacity   = (cache->m_finish - cache->m_start);
  unsigned new_capacity    = curr_capacity * 2;
  unsigned new_entry_capacity = cache->m_entry_capacity * 2;
  unsigned * new_start     = SE_alloc(new_capacity * sizeof(unsigned));
  unsigned * new_finish    = new_start + new_capacity;
  unsigned * old_start     = cache->m_start;
  unsigned * old_finish    = cache->m_finish;
  unsigned words_per_entry = cache->m_words_per_entry;
  unsigned * it;
  unsigned idx             = 0;
  int * mapped_idxs;
  assert(new_entry_capacity * words_per_entry == new_capacity);
  c_int_verbose_message(4, "    expanding cache table, new size: ~a bytes...", new_capacity * sizeof(unsigned));
  cache->m_start = new_start;
  cache->m_finish = new_finish;
  assert(cache->m_finish - cache->m_start == new_capacity);
  cache->m_entry_capacity = new_entry_capacity;
  memset(new_start, 0, new_capacity * sizeof(unsigned));
  // printf("size: %d, words_per_entry: %d \n", cache->m_num_used_entries, words_per_entry);
  mapped_idxs = (int *) GC_malloc_atomic(cache->m_entry_capacity * sizeof(unsigned));
  for (it = old_start; it < old_finish; it += words_per_entry, idx++) {
    assert(idx < cache->m_entry_capacity);
    if (!SC_is_nil_entry(it)) {
      int r = SC_insert_core(cache, it, new_entry_capacity);
      assert(new_start + r * words_per_entry < new_finish);
      mapped_idxs[idx] = r;
      // printf("%d -> %d\n", idx, r);
      assert(r>=0);
    }
    else
      mapped_idxs[idx] = -1;
  }
  sc_idx_notification_handler(mapped_idxs, cache->m_entry_capacity);
  assert(validate(cache));
  // c_simple_verbose_message(4, "    DONE....");
}


int validate(SC_state_cache_ptr cache) {
  unsigned * it;
  unsigned num = 0;
  assert(cache->m_start + cache->m_entry_capacity * cache->m_words_per_entry <= cache->m_finish);
  for(it = cache->m_start; it < cache->m_finish; it += cache->m_words_per_entry)
    if (!SC_is_nil_entry(it))
      num++;
  if (cache->m_num_used_entries != num) {
    printf("num: %d, num used entries: %d\n", num, cache->m_num_used_entries);
    assert(cache->m_num_used_entries == num);
   }
  return 1;
}


int SC_insert_channel(SC_state_cache_ptr cache, SEC_state_entry_channel_ptr channel) {
  int result;
  assert(cache->m_start + cache->m_entry_capacity * cache->m_words_per_entry <= cache->m_finish);
  assert(cache->m_entry_size == SEC_entry_size(channel));
  if (100 * cache->m_num_used_entries >= g_load_balance * cache->m_entry_capacity) {
    /* printf("num_used_entries: %d, entry_capacity: %d\n", cache->m_num_used_entries, cache->m_entry_capacity); */
    // assert(validate(cache));
    SC_expand_table(cache);
  }
  result = SC_insert_core(cache, SEC_entry(channel), cache->m_entry_capacity);
  if (result >= 0)
    cache->m_num_used_entries ++;
  // assert(validate(cache));
  return result;
}

unsigned SC_size(SC_state_cache_ptr cache) {
  return cache->m_num_used_entries;
}

