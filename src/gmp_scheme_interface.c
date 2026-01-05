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

#include <bigloo.h>
#include <gmp.h>
#include <string.h>

/* 
void * GC_malloc(size_t size_in_bytes);
*/
extern obj_t GC_realloc(obj_t old_object, size_t new_size_in_bytes);

void * sgmp_realloc(void * old_object, size_t old_size, size_t new_size)
{
  return (void *) GC_realloc((obj_t) old_object, new_size);
}

void sgmp_dummy_free(void * object, size_t size)
{
  /* do nothing */
}

static mpz_t gMPQtmp1;
static mpz_t gMPQtmp2;
static mpz_t gMPQtmp3;
char * smpq_to_string(mpq_t * num);

int sgmp_init()
{
  mp_set_memory_functions ((void *(*) (size_t))GC_malloc,
               sgmp_realloc,
               sgmp_dummy_free);
  mpz_init(gMPQtmp1);
  mpz_init(gMPQtmp2);
  mpz_init(gMPQtmp3);
  return 0;
}

mpq_t * smpq_new_core(char * str, int base)
{
  mpq_t * result = (mpq_t *) GC_malloc(sizeof(mpq_t));
  mpq_init(*result);
  mpz_init_set_str(gMPQtmp1, str, base);
  mpq_set_z(*result, gMPQtmp1);
  return result;
}

mpq_t * smpq_new(char * str)
{
  return smpq_new_core(str, 10);
}


#define GEN_BINARY_OPERATOR(SCM_FUN_NAME, GMP_FUN_NAME) \
mpq_t * SCM_FUN_NAME(mpq_t * arg1, mpq_t * arg2)        \
{                                                       \
  mpq_t * result = (mpq_t *) GC_malloc(sizeof(mpq_t));  \
  mpq_init(*result);                                    \
  GMP_FUN_NAME(*result, *arg1, *arg2);                  \
  return result;                                        \
}

GEN_BINARY_OPERATOR(smpq_add, mpq_add)
GEN_BINARY_OPERATOR(smpq_sub, mpq_sub)
GEN_BINARY_OPERATOR(smpq_mul, mpq_mul)
GEN_BINARY_OPERATOR(smpq_div, mpq_div)

#define GEN_BINARY_INT_OPERATOR(SCM_FUN_NAME, GMP_FUN_NAME)                                                   \
mpq_t * SCM_FUN_NAME(mpq_t * arg1, mpq_t * arg2)                                                              \
{                                                                                                             \
  mpq_t * result = (mpq_t *) GC_malloc(sizeof(mpq_t));                                                        \
  mpq_init(*result);                                                                                          \
                                                                                                              \
  mpq_get_den(gMPQtmp1, *arg1);                                                                               \
  if (mpz_cmp_si(gMPQtmp1, 1) != 0)                                                                           \
    C_FAILURE("gmp-integer-operator", "invalid rational argument", string_to_bstring(smpq_to_string(arg1)));  \
                                                                                                              \
  mpq_get_den(gMPQtmp1, *arg2);                                                                               \
  if (mpz_cmp_si(gMPQtmp1, 1) != 0)                                                                           \
    C_FAILURE("gmp-integer-operator", "invalid rational argument", string_to_bstring(smpq_to_string(arg2)));  \
                                                                                                              \
  mpq_get_num(gMPQtmp1, *arg1);                                                                               \
  mpq_get_num(gMPQtmp2, *arg2);                                                                               \
  GMP_FUN_NAME(gMPQtmp3, gMPQtmp1, gMPQtmp2);                                                                 \
                                                                                                              \
  mpq_set_z(*result, gMPQtmp3);                                                                               \
                                                                                                              \
  return result;                                                                                              \
}

GEN_BINARY_INT_OPERATOR(smpq_modulo, mpz_mod)
GEN_BINARY_INT_OPERATOR(smpq_idiv, mpz_tdiv_q)

#define GEN_ACCESSOR(SCM_FUN_NAME, GMP_FUN_NAME)                                              \
mpq_t * SCM_FUN_NAME(mpq_t * arg1)                                                            \
{                                                                                             \
  mpq_t * result = (mpq_t *) GC_malloc(sizeof(mpq_t));                                        \
  mpq_init(*result);                                                                          \
                                                                                              \
  GMP_FUN_NAME(gMPQtmp1, *arg1);                                                              \
  mpq_set_z(*result, gMPQtmp1);                                                               \
                                                                                              \
  return result;                                                                              \
}

GEN_ACCESSOR(smpq_get_num, mpq_get_num)
GEN_ACCESSOR(smpq_get_den, mpq_get_den)

int smpq_cmp(mpq_t * arg1, mpq_t * arg2)       
{                                               
  return mpq_cmp(*arg1, *arg2);                               
}

int smpq_hash(mpq_t * arg)
{
  mpq_get_num(gMPQtmp1, *arg);
  return mpz_get_si(gMPQtmp1);
}

int smpq_to_int(mpq_t * arg)
{
  mpq_get_num(gMPQtmp1, *arg);
  return mpz_get_si(gMPQtmp1);
}    

mpq_t * int_to_smpq(int value)
{
  mpq_t * result = (mpq_t *) GC_malloc(sizeof(mpq_t));
  mpq_init(*result);
  mpq_set_si(*result, value, 1);
  return result;
}

char * smpq_to_string(mpq_t * num)
{
  char * str1;
  mpq_get_num(gMPQtmp1, *num);
  str1 = mpz_get_str(0, 10, gMPQtmp1);
  mpq_get_den(gMPQtmp1, *num);
  if (mpz_cmp_si(gMPQtmp1, 1) == 0)
    return str1;
  else {
    char * str2 = mpz_get_str(0, 10, gMPQtmp1);
    int len1 = strlen(str1);
    int len2 = strlen(str2);
    char * result = (char *) GC_malloc(len1 + len2 + 1 /* space for '/' */ + 1);
    memcpy(result, str1, len1);
    result[len1] = '/';
    memcpy(result + len1 + 1, str2, len2);
    result[len1 + len2 + 1] = 0;
    return result;
  }
}
