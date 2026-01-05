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

#include<assert.h>
#include<stdlib.h>
#include<stdio.h>
#include<string.h>

#include"state_entry.h"
#include"state_entry_channel.h"

void * GC_malloc(size_t size_in_bytes);
void * GC_malloc_atomic(size_t size_in_bytes);

SEC_state_entry_channel_ptr SEC_make(unsigned num_bits) {
  SEC_state_entry_channel_ptr result = (SEC_state_entry_channel_ptr) GC_malloc(sizeof(struct SEC_state_entry_channel));
  unsigned num_words = (num_bits / WORD_NUM_BITS) + (num_bits % WORD_NUM_BITS ? 1 : 0);
  unsigned entry_size = num_words * sizeof(unsigned);
  result->m_num_bits = num_bits;
  result->m_num_words = num_words;
  result->m_entry_size = entry_size;
  result->m_entry = SE_alloc(entry_size);
  memset(result->m_entry, 0, entry_size);
  return result;
}

int SEC_get_entry_bit(SEC_state_entry_channel_ptr channel, unsigned pos)
{
  unsigned idx = pos / WORD_NUM_BITS;                           
  unsigned offset = pos % WORD_NUM_BITS;                          
  unsigned int mask = 1 << offset;              
  return (channel->m_entry[idx] & mask) > 0;
}

void SEC_print_state_entry(SEC_state_entry_channel_ptr channel) {
  unsigned i;
  printf("[");
  for(i = 0; i < channel->m_num_bits; i++) {
    if (i > 0)
      printf(" ");
    printf("%d", SEC_get_entry_bit(channel, i));
  }
  printf("]");
}

void SEC_output_reset(SEC_state_entry_channel_ptr channel) {
  channel->m_bit_idx = 0;
  channel->m_bit_offset = 0;
}

void SEC_input_reset(SEC_state_entry_channel_ptr channel) {
  memset(channel->m_entry, 0, channel->m_entry_size);
  channel->m_bit_idx = 0;
  channel->m_bit_offset = 0;
}

#define ADVANCE_ONE_BIT(channel) {              \
  channel->m_bit_offset++;                      \
  if (channel->m_bit_offset >= WORD_NUM_BITS) { \
    channel->m_bit_offset = 0;                  \
    channel->m_bit_idx    ++;                   \
  }                                             \
} ((void)0)

void SEC_add_bit(SEC_state_entry_channel_ptr channel, int val) {
  assert(channel->m_bit_idx < channel->m_num_words);
  if (val) {
    unsigned idx  = channel->m_bit_idx;
    unsigned mask = 1 << channel->m_bit_offset;
    channel->m_entry[idx] = channel->m_entry[idx] | mask;
  }
  ADVANCE_ONE_BIT(channel);
}

void SEC_add_num(SEC_state_entry_channel_ptr channel, unsigned val, unsigned num_bits) {
  if (num_bits > 0) {
    unsigned * entry   = channel->m_entry;
    unsigned   idx     = channel->m_bit_idx;
    unsigned   offset  = channel->m_bit_offset;
    unsigned   mask    = val << offset;
    assert(idx < channel->m_num_words);
    entry[idx] = entry[idx] | mask;
    channel->m_bit_offset += num_bits;
    if (channel->m_bit_offset >= WORD_NUM_BITS) {
      unsigned bits_placed = WORD_NUM_BITS - offset;
      channel->m_bit_offset = channel->m_bit_offset - WORD_NUM_BITS;
      channel->m_bit_idx    ++;
      if (bits_placed < num_bits) {
        unsigned mask = val >> bits_placed;
        assert(idx+1 < channel->m_num_words);
        entry[idx+1] = mask;
      }
    }
  }
}

void SEC_add_aligned_32_bit(SEC_state_entry_channel_ptr channel, unsigned val) {
  unsigned * entry   = channel->m_entry;
  unsigned   idx     = channel->m_bit_idx;
  assert(channel->m_bit_offset == 0);
  entry[idx] = val;
  channel->m_bit_idx++;
}

int SEC_read_bit(SEC_state_entry_channel_ptr channel) {
  unsigned idx;
  unsigned mask;
  int result;
  assert(channel->m_bit_idx < channel->m_num_words);
  idx  = channel->m_bit_idx;
  mask = 1 << channel->m_bit_offset;
  result = (channel->m_entry[idx] & mask) > 0;
  ADVANCE_ONE_BIT(channel);
  return result;
}

unsigned SEC_read_num(SEC_state_entry_channel_ptr channel, unsigned num_bits) {
  if (num_bits > 0) {
    unsigned * entry   = channel->m_entry;
    unsigned   idx     = channel->m_bit_idx;
    unsigned   offset  = channel->m_bit_offset;
    unsigned   mask    = (1 << num_bits) - 1;
    unsigned   val     = (entry[idx] >> offset) & mask;
    assert(channel->m_bit_idx < channel->m_num_words);
    channel->m_bit_offset += num_bits;
    if (channel->m_bit_offset >= WORD_NUM_BITS) {
      unsigned bits_read = WORD_NUM_BITS - offset;
      unsigned remaining_bits = num_bits - bits_read;
      channel->m_bit_offset = channel->m_bit_offset - WORD_NUM_BITS;
      channel->m_bit_idx    ++;
      if (bits_read < num_bits) {
        unsigned mask;
        assert(idx+1 < channel->m_num_words);
        mask = (1 << remaining_bits) - 1;
        val += (entry[idx+1] & mask) << bits_read; 
      }
    }
    return val;
  }
  else
    return 0;
}

unsigned SEC_read_aligned_32_bit(SEC_state_entry_channel_ptr channel) {
  unsigned * entry   = channel->m_entry;
  unsigned   idx     = channel->m_bit_idx;
  unsigned result    = entry[idx];
  assert(channel->m_bit_offset == 0);
  channel->m_bit_idx++;
  return result;
}

void SEC_consume_32_bit_words(SEC_state_entry_channel_ptr channel, unsigned num_words) {
  assert(channel->m_bit_offset == 0);
  channel->m_bit_idx += num_words;
}

unsigned SEC_entry_size(SEC_state_entry_channel_ptr channel) {
  return channel->m_entry_size;
}

unsigned SEC_num_words(SEC_state_entry_channel_ptr channel) {
  return channel->m_num_words;
}

unsigned * SEC_entry(SEC_state_entry_channel_ptr channel) {
  return channel->m_entry;
}

unsigned SEC_get_num_bits(SEC_state_entry_channel_ptr channel) {
  return channel->m_num_bits;
}

int SEC_equal(SEC_state_entry_channel_ptr channel1, SEC_state_entry_channel_ptr channel2) {
  return SE_eq(channel1->m_entry, channel2->m_entry, channel1->m_num_words);
}

void SEC_copy(SEC_state_entry_channel_ptr target_channel, SEC_state_entry_channel_ptr source_channel) {
  assert(target_channel->m_entry_size == source_channel->m_entry_size);
  memcpy(target_channel->m_entry, source_channel->m_entry, source_channel->m_entry_size);
}

SEC_state_entry_channel_ptr SEC_make_copy(SEC_state_entry_channel_ptr channel) {
  SEC_state_entry_channel_ptr result = SEC_make(channel->m_num_bits);
  SEC_copy(result, channel);
  return result;
}
