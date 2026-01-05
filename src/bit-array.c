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

#include <assert.h>
#include <string.h>

void * GC_malloc(size_t size_in_bytes);
void * GC_realloc(void * old_object, size_t new_size_in_bytes);

typedef struct BA_bitarray * BA_bitarray_ptr;

struct BA_bitarray {
  char * data;
  int size;
  int numbits;
};

void BA_adjust_num_bits(BA_bitarray_ptr bitarray, int pos)
{
  int requestedSize;
  if (pos >= bitarray->numbits) {
    bitarray->numbits = pos + 1;
    requestedSize = (bitarray->numbits / 8) + (bitarray->numbits % 8 ? 1 : 0);
    while (requestedSize > bitarray->size) {
      bitarray->data = (char *) GC_realloc(bitarray->data, bitarray->size * 2);
      memset(bitarray->data + bitarray->size, 0, bitarray->size);
      bitarray->size *=2;
    }
  }
}

BA_bitarray_ptr BA_make(int size)
{
  BA_bitarray_ptr bitarray = (BA_bitarray_ptr) GC_malloc(sizeof(struct BA_bitarray));
  bitarray->numbits = 0;
  bitarray->size = size;
  bitarray->data = (char *) GC_malloc(size);
  memset(bitarray->data, 0, bitarray->size);
  return bitarray;
}

BA_bitarray_ptr BA_copy(BA_bitarray_ptr source)
{
  BA_bitarray_ptr bitarray = (BA_bitarray_ptr) GC_malloc(sizeof(struct BA_bitarray));
  bitarray->numbits = source->numbits;
  bitarray->size = source->size;
  bitarray->data = (char *) GC_malloc(bitarray->size);
  memcpy(bitarray->data, source->data, bitarray->size);
  return bitarray;
}

int BA_get_num_bits(BA_bitarray_ptr bitarray)
{
  return bitarray->numbits;
}

int BA_get_size(BA_bitarray_ptr bitarray)
{
  return bitarray->size;
}

#define COMPUTE_OFFSET_MASK()                   \
  int idx = pos / 8;                            \
  int offset = pos % 8;                         \
  unsigned int mask = 1 << offset;              \
    ((void) 0)

int BA_zero_array(BA_bitarray_ptr bitarray)
{
  int i;
  for (i = 0; i < bitarray->size; i++)
    if (bitarray->data[i] != 0)
      return 0;
  return 1;
}

int BA_get_bit(BA_bitarray_ptr bitarray, int pos)
{
  COMPUTE_OFFSET_MASK();
  assert(pos < bitarray->numbits);
  assert(idx < bitarray->size);
  return (bitarray->data[idx] & mask) > 0;
}

void BA_set_bit(BA_bitarray_ptr bitarray, int pos)
{
  COMPUTE_OFFSET_MASK();
  BA_adjust_num_bits(bitarray, pos);
  assert(idx < bitarray->size);
  bitarray->data[idx] = bitarray->data[idx] | mask;
}

void BA_reset_bit(BA_bitarray_ptr bitarray, int pos)
{
  COMPUTE_OFFSET_MASK();
  BA_adjust_num_bits(bitarray, pos);
  assert(idx < bitarray->size);
  mask = ~mask;
  bitarray->data[idx] = bitarray->data[idx] & mask;
}

void BA_append0(BA_bitarray_ptr bitarray)
{
  BA_reset_bit(bitarray, bitarray->numbits);
}

void BA_append1(BA_bitarray_ptr bitarray)
{
  BA_set_bit(bitarray, bitarray->numbits);
}

void BA_reset(BA_bitarray_ptr bitarray)
{
  memset(bitarray->data, 0, bitarray->numbits);
  bitarray->numbits = 0;
}

int BA_next(BA_bitarray_ptr bitarray)
{
  while(bitarray->numbits > 0 && BA_get_bit(bitarray, bitarray->numbits - 1)) {
    BA_reset_bit(bitarray, bitarray->numbits-1);
    bitarray->numbits--;
  }
  if (bitarray->numbits == 0)
    return 0;
  BA_set_bit(bitarray, bitarray->numbits - 1);
  return 1;
}

BA_bitarray_ptr BA_get_next(BA_bitarray_ptr bitarray)
{
  BA_bitarray_ptr result = BA_copy(bitarray);
  if (BA_next(result))
    return result;
  else
    return NULL;
}

