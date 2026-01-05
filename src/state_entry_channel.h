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

#ifndef STATE_ENTRY_CHANNEL_H
#define STATE_ENTRY_CHANNEL_H

struct SEC_state_entry_channel {
  unsigned * m_entry;
  unsigned   m_num_bits;
  unsigned   m_num_words;
  unsigned   m_entry_size;
  unsigned   m_bit_idx;
  unsigned   m_bit_offset;
};

typedef struct SEC_state_entry_channel * SEC_state_entry_channel_ptr;

SEC_state_entry_channel_ptr SEC_make(unsigned num_bits);
int SEC_get_entry_bit(SEC_state_entry_channel_ptr channel, unsigned pos);
void SEC_print_state_entry(SEC_state_entry_channel_ptr channel);
void SEC_input_reset(SEC_state_entry_channel_ptr channel);
void SEC_output_reset(SEC_state_entry_channel_ptr channel);
void SEC_add_bit(SEC_state_entry_channel_ptr channel, int val);
void SEC_add_num(SEC_state_entry_channel_ptr channel, unsigned val, unsigned num_bits);
void SEC_add_aligned_32_bit(SEC_state_entry_channel_ptr channel, unsigned val);
int SEC_read_bit(SEC_state_entry_channel_ptr channel);
unsigned SEC_read_num(SEC_state_entry_channel_ptr channel, unsigned num_bits);
unsigned SEC_read_aligned_32_bit(SEC_state_entry_channel_ptr channel);
void SEC_consume_32_bit_words(SEC_state_entry_channel_ptr channel, unsigned num_words);
unsigned SEC_entry_size(SEC_state_entry_channel_ptr channel);
unsigned SEC_num_words(SEC_state_entry_channel_ptr channel);
unsigned * SEC_entry(SEC_state_entry_channel_ptr channel);
int SEC_equal(SEC_state_entry_channel_ptr channel1, SEC_state_entry_channel_ptr channel2);
void SEC_copy(SEC_state_entry_channel_ptr target_channel, SEC_state_entry_channel_ptr source_channel);
SEC_state_entry_channel_ptr SEC_make_copy(SEC_state_entry_channel_ptr channel);

#endif /* STATE_ENTRY_CHANNEL_H */
