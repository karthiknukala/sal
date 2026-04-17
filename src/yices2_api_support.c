#include <stdlib.h>
#include <stdint.h>

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

int bgl_yices_parse_type(char *type_string) {
  return yices_parse_type(type_string);
}

int bgl_yices_parse_term(char *term_string) {
  return yices_parse_term(term_string);
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

int bgl_yices_assert_formula(void *ctx, int term_id) {
  return yices_assert_formula((context_t *)ctx, (term_t)term_id);
}

void *bgl_yices_get_model(void *ctx, int keep_subst) {
  return yices_get_model((context_t *)ctx, keep_subst);
}

void bgl_yices_free_model(void *model) {
  yices_free_model((model_t *)model);
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

void bgl_yices_term_array_free(term_t *array) {
  free(array);
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
