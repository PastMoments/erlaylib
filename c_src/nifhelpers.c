#include "erl_nif.h"
#include "raylib.h"

bool enif_get_tuple_as_color(ErlNifEnv* env, const ERL_NIF_TERM tuple, Color* color) {
    const ERL_NIF_TERM* actual_tuple;
    int actual_arity;
    if (!enif_get_tuple(env, tuple, &actual_arity, &actual_tuple) || 4 != actual_arity) {
	    return false;
    }

    unsigned int r;
    unsigned int g;
    unsigned int b;
    unsigned int a;
    if (!enif_get_uint(env, actual_tuple[0], &r) || r >= 256 ||
        !enif_get_uint(env, actual_tuple[1], &g) || g >= 256 ||
        !enif_get_uint(env, actual_tuple[2], &b) || b >= 256 ||
        !enif_get_uint(env, actual_tuple[3], &a) || a >= 256) {
	    return false;
    }
    *color =  (Color){r,g,b,a};
    return true;
}

ERL_NIF_TERM enif_make_atom_from_bool(ErlNifEnv* env, bool value) {
    return enif_make_atom(env, value ? "true" : "false");
}

ERL_NIF_TERM enif_make_atom_ok(ErlNifEnv* env) {
    return enif_make_atom(env, "ok");
}