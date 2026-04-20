#include <stdlib.h>
#include <stdint.h>

#include <gmp.h>
#include <bigloo.h>
#include <bigloo_string.h>
#include <yices.h>

int bgl_yices_pointer_null(void *ptr) {
  return ptr == NULL;
}

void bgl_yices_init(void) {
  yices_init();
}

void bgl_yices_exit(void) {
  yices_exit();
}

void bgl_yices_reset(void) {
  yices_reset();
}

int bgl_yices_has_mcsat(void) {
  return yices_has_mcsat();
}

obj_t bgl_yices_version_string(void) {
  if (yices_version == NULL) {
    return BFALSE;
  }
  return string_to_bstring((char *) yices_version);
}

void *bgl_yices_new_config(void) {
  return yices_new_config();
}

void bgl_yices_free_config(void *config) {
  yices_free_config((ctx_config_t *)config);
}

int bgl_yices_set_config(void *config, char *name, char *value) {
  return yices_set_config((ctx_config_t *)config, name, value);
}

void *bgl_yices_new_context(void *config) {
  return yices_new_context((ctx_config_t *)config);
}

void bgl_yices_free_context(void *ctx) {
  yices_free_context((context_t *)ctx);
}

void bgl_yices_reset_context(void *ctx) {
  yices_reset_context((context_t *)ctx);
}

int bgl_yices_parse_type(char *type_string) {
  return yices_parse_type(type_string);
}

int bgl_yices_parse_term(char *term_string) {
  return yices_parse_term(term_string);
}

int bgl_yices_parse_rational(char *rational_string) {
  return yices_parse_rational(rational_string);
}

int bgl_yices_true(void) {
  return yices_true();
}

int bgl_yices_false(void) {
  return yices_false();
}

int bgl_yices_new_uninterpreted_type(void) {
  return yices_new_uninterpreted_type();
}

int bgl_yices_set_type_name(int type_id, char *name) {
  return yices_set_type_name((type_t)type_id, name);
}

int bgl_yices_new_uninterpreted_term(int type_id) {
  return yices_new_uninterpreted_term((type_t)type_id);
}

int bgl_yices_set_term_name(int term_id, char *name) {
  return yices_set_term_name((term_t)term_id, name);
}

int bgl_yices_not(int term_id) {
  return yices_not((term_t)term_id);
}

int bgl_yices_neg(int term_id) {
  return yices_neg((term_t)term_id);
}

int bgl_yices_eq(int left, int right) {
  return yices_eq((term_t)left, (term_t)right);
}

int bgl_yices_neq(int left, int right) {
  return yices_neq((term_t)left, (term_t)right);
}

int bgl_yices_iff(int left, int right) {
  return yices_iff((term_t)left, (term_t)right);
}

int bgl_yices_implies(int left, int right) {
  return yices_implies((term_t)left, (term_t)right);
}

int bgl_yices_ite(int cond, int then_term, int else_term) {
  return yices_ite((term_t)cond, (term_t)then_term, (term_t)else_term);
}

int bgl_yices_or_terms(int count, void *term_array) {
  return yices_or((uint32_t)count, (term_t *)term_array);
}

int bgl_yices_and_terms(int count, void *term_array) {
  return yices_and((uint32_t)count, (term_t *)term_array);
}

int bgl_yices_application(int fun, int count, void *term_array) {
  return yices_application((term_t)fun, (uint32_t)count, (const term_t *)term_array);
}

int bgl_yices_update(int fun, int count, void *term_array, int new_value) {
  return yices_update((term_t)fun,
                      (uint32_t)count,
                      (const term_t *)term_array,
                      (term_t)new_value);
}

int bgl_yices_add(int left, int right) {
  return yices_add((term_t)left, (term_t)right);
}

int bgl_yices_sub(int left, int right) {
  return yices_sub((term_t)left, (term_t)right);
}

int bgl_yices_mul(int left, int right) {
  return yices_mul((term_t)left, (term_t)right);
}

int bgl_yices_sum_terms(int count, void *term_array) {
  return yices_sum((uint32_t)count, (const term_t *)term_array);
}

int bgl_yices_product_terms(int count, void *term_array) {
  return yices_product((uint32_t)count, (const term_t *)term_array);
}

int bgl_yices_division(int left, int right) {
  return yices_division((term_t)left, (term_t)right);
}

int bgl_yices_idiv(int left, int right) {
  return yices_idiv((term_t)left, (term_t)right);
}

int bgl_yices_arith_lt_atom(int left, int right) {
  return yices_arith_lt_atom((term_t)left, (term_t)right);
}

int bgl_yices_arith_leq_atom(int left, int right) {
  return yices_arith_leq_atom((term_t)left, (term_t)right);
}

int bgl_yices_arith_gt_atom(int left, int right) {
  return yices_arith_gt_atom((term_t)left, (term_t)right);
}

int bgl_yices_arith_geq_atom(int left, int right) {
  return yices_arith_geq_atom((term_t)left, (term_t)right);
}

int bgl_yices_arith_lt0_atom(int term_id) {
  return yices_arith_lt0_atom((term_t)term_id);
}

int bgl_yices_arith_leq0_atom(int term_id) {
  return yices_arith_leq0_atom((term_t)term_id);
}

int bgl_yices_arith_gt0_atom(int term_id) {
  return yices_arith_gt0_atom((term_t)term_id);
}

int bgl_yices_arith_geq0_atom(int term_id) {
  return yices_arith_geq0_atom((term_t)term_id);
}

int bgl_yices_assert_formula(void *ctx, int term_id) {
  return yices_assert_formula((context_t *)ctx, (term_t)term_id);
}

int bgl_yices_push(void *ctx) {
  return yices_push((context_t *)ctx);
}

int bgl_yices_pop(void *ctx) {
  return yices_pop((context_t *)ctx);
}

int bgl_yices_check_context(void *ctx) {
  return (int)yices_check_context((context_t *)ctx, NULL);
}

int bgl_yices_check_context_with_model(void *ctx, void *model, int num_terms, void *term_array) {
  return (int)yices_check_context_with_model((context_t *)ctx,
                                             NULL,
                                             (model_t *)model,
                                             (uint32_t)num_terms,
                                             (const term_t *)term_array);
}

void *bgl_yices_get_model(void *ctx, int keep_subst) {
  return yices_get_model((context_t *)ctx, keep_subst);
}

void bgl_yices_free_model(void *model) {
  yices_free_model((model_t *)model);
}

void *bgl_yices_model_from_map(int num_terms, void *var_array, void *map_array) {
  return yices_model_from_map((uint32_t)num_terms,
                              (const term_t *)var_array,
                              (const term_t *)map_array);
}

int bgl_yices_get_model_interpolant(void *ctx) {
  return yices_get_model_interpolant((context_t *)ctx);
}

int bgl_yices_get_value_as_term(void *model, int term_id) {
  return yices_get_value_as_term((model_t *)model, (term_t)term_id);
}

int bgl_yices_generalize_model_array(void *model, int num_formulas, void *formula_array,
                                     int num_elims, void *elim_array,
                                     int mode, void *vector) {
  return yices_generalize_model_array((model_t *)model,
                                      (uint32_t)num_formulas,
                                      (const term_t *)formula_array,
                                      (uint32_t)num_elims,
                                      (const term_t *)elim_array,
                                      (yices_gen_mode_t)mode,
                                      (term_vector_t *)vector);
}

obj_t bgl_yices_error_string(void) {
  char *msg = yices_error_string();
  if (msg == NULL) {
    return BFALSE;
  }
  obj_t result = string_to_bstring(msg);
  yices_free_string(msg);
  return result;
}

obj_t bgl_yices_term_to_string(int32_t term) {
  char *msg = yices_term_to_string(term, UINT32_MAX, UINT32_MAX, 0);
  if (msg == NULL) {
    return BFALSE;
  }
  obj_t result = string_to_bstring(msg);
  yices_free_string(msg);
  return result;
}

int bgl_yices_term_constructor(int32_t term) {
  return (int)yices_term_constructor((term_t)term);
}

int bgl_yices_term_num_children(int32_t term) {
  return yices_term_num_children((term_t)term);
}

int32_t bgl_yices_term_child(int32_t term, int index) {
  return yices_term_child((term_t)term, index);
}

obj_t bgl_yices_get_term_name(int32_t term) {
  const char *name = yices_get_term_name((term_t)term);
  if (name == NULL) {
    return BFALSE;
  }
  return string_to_bstring((char *)name);
}

int32_t bgl_yices_sum_component_term(int32_t term, int index) {
  mpq_t coeff;
  term_t component;
  int code;

  mpq_init(coeff);
  code = yices_sum_component((term_t)term, index, coeff, &component);
  mpq_clear(coeff);

  if (code < 0) {
    return NULL_TERM;
  }

  return component;
}

int32_t bgl_yices_product_component_term(int32_t term, int index) {
  term_t factor;
  uint32_t exponent;
  int code;

  code = yices_product_component((term_t)term, index, &factor, &exponent);
  if (code < 0) {
    return NULL_TERM;
  }

  return factor;
}

term_t *bgl_yices_term_array_alloc(int count) {
  if (count <= 0) {
    return NULL;
  }
  return (term_t *)malloc((size_t)count * sizeof(term_t));
}

void bgl_yices_term_array_set(term_t *array, int index, int32_t term) {
  if (array != NULL && index >= 0) {
    array[index] = term;
  }
}

int32_t bgl_yices_term_array_ref(term_t *array, int index) {
  if (array == NULL || index < 0) {
    return NULL_TERM;
  }
  return array[index];
}

void bgl_yices_term_array_free(term_t *array) {
  free(array);
}

int bgl_yices_term_array_value(void *model, int count, void *input_array, void *output_array) {
  return yices_term_array_value((model_t *)model,
                                (uint32_t)count,
                                (const term_t *)input_array,
                                (term_t *)output_array);
}

term_vector_t *bgl_yices_term_vector_new(void) {
  term_vector_t *vec = (term_vector_t *)malloc(sizeof(term_vector_t));
  if (vec == NULL) {
    return NULL;
  }
  yices_init_term_vector(vec);
  return vec;
}

void bgl_yices_term_vector_delete(term_vector_t *vec) {
  if (vec != NULL) {
    yices_delete_term_vector(vec);
    free(vec);
  }
}

void bgl_yices_term_vector_reset(term_vector_t *vec) {
  if (vec != NULL) {
    yices_reset_term_vector(vec);
  }
}

int bgl_yices_term_vector_size(term_vector_t *vec) {
  if (vec == NULL) {
    return 0;
  }
  return (int)vec->size;
}

int32_t bgl_yices_term_vector_ref(term_vector_t *vec, int index) {
  if (vec == NULL || index < 0 || (uint32_t)index >= vec->size) {
    return NULL_TERM;
  }
  return vec->data[index];
}

int bgl_yices_check_context_default(context_t *ctx) {
  return (int)yices_check_context(ctx, NULL);
}
