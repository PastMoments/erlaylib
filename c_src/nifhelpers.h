#include "erl_nif.h"

bool enif_get_tuple_as_color(ErlNifEnv* env, const ERL_NIF_TERM tuple, Color* color);

ERL_NIF_TERM enif_make_atom_from_bool(ErlNifEnv* env, bool value);

ERL_NIF_TERM enif_make_atom_ok(ErlNifEnv* env);