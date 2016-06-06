#include "erl_nif.h"
#include "murmur3.h"
#include <string.h>

extern ERL_NIF_TERM
ordered_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

extern ERL_NIF_TERM
unordered_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

int
extract_hashes(ErlNifEnv *env, ERL_NIF_TERM list, uint32_t *hashes, uint32_t length);

int
upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info);

extern ERL_NIF_TERM
ordered_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t ret;
  uint32_t length;

  if (!enif_get_list_length(env, argv[0], &length)) {
    return enif_make_badarg(env);
  }

  ERL_NIF_TERM list = argv[0];
  uint32_t* hashes = (uint32_t *) malloc(sizeof(uint32_t) * length);

  if(!extract_hashes(env, list, hashes, length)) {
    return enif_make_badarg(env);
  };

  ret = murmur_ordered_coll(hashes, length);

  free(hashes);

  return enif_make_int(env, ret);
}

extern ERL_NIF_TERM
unordered_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  uint32_t ret;
  uint32_t length;

  if (!enif_get_list_length(env, argv[0], &length)) {
    return enif_make_badarg(env);
  }

  ERL_NIF_TERM list = argv[0];
  uint32_t* hashes = (uint32_t *) malloc(sizeof(uint32_t) * length);

  if(!extract_hashes(env, list, hashes, length)) {
    return enif_make_badarg(env);
  };

  ret = murmur_unordered_coll(hashes, length);

  free(hashes);

  return enif_make_int(env, ret);
}

int
extract_hashes(ErlNifEnv *env, ERL_NIF_TERM list, uint32_t *hashes, uint32_t length) {
  ERL_NIF_TERM head;
  ERL_NIF_TERM tail;
  int hash;


  for(int i = 0; i < length; i++) {
    if(!enif_get_list_cell(env, list, &head, &tail)) {
      return 0;
    }
    if(!enif_get_int(env, head, &hash)) {
      return 0;
    }
    hashes[i] = hash;
    list = tail;
  }

  return 1;
}

// Provide upgrade function that does nothing
int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
  return 0;
}

static ErlNifFunc nif_funcs[] = {
  {"ordered_nif", 1, ordered_nif},
  {"unordered_nif", 1, unordered_nif}
};

ERL_NIF_INIT(clj_murmur3, nif_funcs, NULL, NULL, &upgrade, NULL);
