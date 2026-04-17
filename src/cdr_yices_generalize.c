#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include <yices.h>

typedef struct {
  char **data;
  uint32_t size;
  uint32_t capacity;
} string_vec_t;

typedef struct {
  term_t *data;
  uint32_t size;
  uint32_t capacity;
} term_id_vec_t;

typedef struct {
  char **names;
  term_t *terms;
  uint32_t size;
  uint32_t capacity;
} named_term_vec_t;

static void die(const char *msg) {
  fprintf(stderr, "cdr-yices-generalize: %s\n", msg);
  exit(1);
}

static void die_yices(const char *msg) {
  char *details = yices_error_string();
  if (details != NULL) {
    fprintf(stderr, "cdr-yices-generalize: %s: %s\n", msg, details);
    free(details);
  } else {
    fprintf(stderr, "cdr-yices-generalize: %s\n", msg);
  }
  exit(1);
}

static char *xstrdup(const char *s) {
  size_t n = strlen(s) + 1;
  char *copy = (char *) malloc(n);
  if (copy == NULL) {
    die("out of memory");
  }
  memcpy(copy, s, n);
  return copy;
}

static void string_vec_push(string_vec_t *vec, const char *value) {
  if (vec->size == vec->capacity) {
    uint32_t new_capacity = vec->capacity == 0 ? 8 : vec->capacity * 2;
    char **new_data = (char **) realloc(vec->data, new_capacity * sizeof(char *));
    if (new_data == NULL) {
      die("out of memory");
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
  }
  vec->data[vec->size++] = xstrdup(value);
}

static void term_id_vec_push(term_id_vec_t *vec, term_t term) {
  if (vec->size == vec->capacity) {
    uint32_t new_capacity = vec->capacity == 0 ? 8 : vec->capacity * 2;
    term_t *new_data = (term_t *) realloc(vec->data, new_capacity * sizeof(term_t));
    if (new_data == NULL) {
      die("out of memory");
    }
    vec->data = new_data;
    vec->capacity = new_capacity;
  }
  vec->data[vec->size++] = term;
}

static void named_term_vec_push(named_term_vec_t *vec, const char *name, term_t term) {
  if (vec->size == vec->capacity) {
    uint32_t new_capacity = vec->capacity == 0 ? 8 : vec->capacity * 2;
    char **new_names = (char **) realloc(vec->names, new_capacity * sizeof(char *));
    term_t *new_terms = (term_t *) realloc(vec->terms, new_capacity * sizeof(term_t));
    if (new_names == NULL || new_terms == NULL) {
      die("out of memory");
    }
    vec->names = new_names;
    vec->terms = new_terms;
    vec->capacity = new_capacity;
  }
  vec->names[vec->size] = xstrdup(name);
  vec->terms[vec->size] = term;
  vec->size += 1;
}

static term_t named_term_lookup(const named_term_vec_t *vec, const char *name) {
  uint32_t i;
  for (i = 0; i < vec->size; ++i) {
    if (strcmp(vec->names[i], name) == 0) {
      return vec->terms[i];
    }
  }
  return NULL_TERM;
}

static int string_vec_contains(const string_vec_t *vec, const char *value) {
  uint32_t i;
  for (i = 0; i < vec->size; ++i) {
    if (strcmp(vec->data[i], value) == 0) {
      return 1;
    }
  }
  return 0;
}

static void normalize_inline(char *s) {
  char *readp = s;
  char *writep = s;
  int in_space = 0;
  while (*readp != '\0') {
    char c = *readp++;
    if (c == '\n' || c == '\r' || c == '\t') {
      c = ' ';
    }
    if (c == ' ') {
      if (in_space) {
        continue;
      }
      in_space = 1;
    } else {
      in_space = 0;
    }
    *writep++ = c;
  }
  if (writep > s && writep[-1] == ' ') {
    --writep;
  }
  *writep = '\0';
}

static void free_string_vec(string_vec_t *vec) {
  uint32_t i;
  for (i = 0; i < vec->size; ++i) {
    free(vec->data[i]);
  }
  free(vec->data);
}

static void free_named_term_vec(named_term_vec_t *vec) {
  uint32_t i;
  for (i = 0; i < vec->size; ++i) {
    free(vec->names[i]);
  }
  free(vec->names);
  free(vec->terms);
}

int main(int argc, char **argv) {
  FILE *input;
  char line[32768];
  const char *logic = "QF_UFNRA";
  string_vec_t keep_names = {0};
  string_vec_t formula_strings = {0};
  named_term_vec_t decls = {0};
  term_id_vec_t formulas = {0};
  term_id_vec_t elim = {0};
  ctx_config_t *config;
  context_t *ctx;
  model_t *model;
  term_vector_t generalized;
  smt_status_t status;
  uint32_t i;
  int32_t code;

  if (argc != 2) {
    die("usage: cdr-yices-generalize <input-file>");
  }

  input = fopen(argv[1], "r");
  if (input == NULL) {
    die("failed to open input file");
  }

  yices_init();

  while (fgets(line, sizeof(line), input) != NULL) {
    char *nl = strchr(line, '\n');
    char *kind;
    char *field1;
    char *field2;
    if (nl != NULL) {
      *nl = '\0';
    }
    if (line[0] == '\0') {
      continue;
    }
    kind = strtok(line, "\t");
    field1 = strtok(NULL, "\t");
    field2 = strtok(NULL, "");
    if (kind == NULL) {
      continue;
    }
    if (strcmp(kind, "logic") == 0) {
      if (field1 == NULL) {
        die("malformed logic line");
      }
      logic = xstrdup(field1);
    } else if (strcmp(kind, "sort") == 0) {
      type_t tau;
      if (field1 == NULL) {
        die("malformed sort line");
      }
      tau = yices_new_uninterpreted_type();
      if (tau < 0) {
        die_yices("failed to create uninterpreted type");
      }
      if (yices_set_type_name(tau, field1) < 0) {
        die_yices("failed to name uninterpreted type");
      }
    } else if (strcmp(kind, "decl") == 0) {
      type_t tau;
      term_t var;
      if (field1 == NULL || field2 == NULL) {
        die("malformed decl line");
      }
      tau = yices_parse_type(field2);
      if (tau < 0) {
        die_yices("failed to parse declaration type");
      }
      var = yices_new_uninterpreted_term(tau);
      if (var < 0) {
        die_yices("failed to create declaration");
      }
      if (yices_set_term_name(var, field1) < 0) {
        die_yices("failed to name declaration");
      }
      named_term_vec_push(&decls, field1, var);
    } else if (strcmp(kind, "formula") == 0) {
      if (field1 == NULL) {
        die("malformed formula line");
      }
      string_vec_push(&formula_strings, field1);
    } else if (strcmp(kind, "keep") == 0) {
      if (field1 == NULL) {
        die("malformed keep line");
      }
      string_vec_push(&keep_names, field1);
    } else {
      die("unknown input directive");
    }
  }

  fclose(input);

  config = yices_new_config();
  if (config == NULL) {
    die_yices("failed to allocate Yices context configuration");
  }
  (void) logic;
  if (yices_set_config(config, "solver-type", "mcsat") < 0) {
    die_yices("failed to enable Yices MCSAT mode");
  }
  ctx = yices_new_context(config);
  if (ctx == NULL) {
    die_yices("failed to create Yices context");
  }
  yices_free_config(config);
  config = NULL;

  for (i = 0; i < formula_strings.size; ++i) {
    term_t formula = yices_parse_term(formula_strings.data[i]);
    if (formula < 0) {
      die_yices("failed to parse formula");
    }
    if (yices_assert_formula(ctx, formula) < 0) {
      die_yices("failed to assert formula");
    }
    term_id_vec_push(&formulas, formula);
  }

  status = yices_check_context(ctx, NULL);
  if (status != YICES_STATUS_SAT) {
    die("generalization query is not satisfiable");
  }

  model = yices_get_model(ctx, 1);
  if (model == NULL) {
    die_yices("failed to get model");
  }

  for (i = 0; i < decls.size; ++i) {
    if (!string_vec_contains(&keep_names, decls.names[i])) {
      term_id_vec_push(&elim, decls.terms[i]);
    }
  }

  yices_init_term_vector(&generalized);
  code = yices_generalize_model_array(model,
                                      formulas.size,
                                      formulas.data,
                                      elim.size,
                                      elim.data,
                                      YICES_GEN_DEFAULT,
                                      &generalized);
  if (code < 0) {
    yices_reset_term_vector(&generalized);
    code = yices_generalize_model_array(model,
                                        formulas.size,
                                        formulas.data,
                                        elim.size,
                                        elim.data,
                                        YICES_GEN_BY_SUBST,
                                        &generalized);
    if (code < 0) {
      die_yices("failed to generalize model");
    }
  }

  if (generalized.size == 0) {
    puts("true");
  } else {
    for (i = 0; i < generalized.size; ++i) {
      char *term_str = yices_term_to_string(generalized.data[i], UINT32_MAX, UINT32_MAX, 0);
      if (term_str == NULL) {
        die_yices("failed to pretty print generalized term");
      }
      normalize_inline(term_str);
      puts(term_str);
      yices_free_string(term_str);
    }
  }

  yices_delete_term_vector(&generalized);
  yices_free_model(model);
  yices_free_context(ctx);
  yices_exit();

  free_string_vec(&keep_names);
  free_string_vec(&formula_strings);
  free_named_term_vec(&decls);
  free(formulas.data);
  free(elim.data);

  return 0;
}
