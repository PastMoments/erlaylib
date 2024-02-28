#include "erl_nif.h"
#include "raylib.h"

#include "nifhelpers.h"

#include "nif_funcs.h"

DECLARE_NIF_FUNC(beginDrawing) {
    BeginDrawing();
    return enif_make_atom_ok(env);
}

DECLARE_NIF_FUNC(endDrawing) {
    EndDrawing();
    return enif_make_atom_ok(env);
}

DECLARE_NIF_FUNC(clearBackground) {
    Color color;
    if (!enif_get_tuple_as_color(env, argv[0], &color)) {
	    return enif_make_badarg(env);
    }
    ClearBackground(color);
    return enif_make_atom_ok(env);
}

DECLARE_NIF_FUNC(drawText) {
    char text[256];
    int posX;
    int posY;
    int fontSize;
    Color color;
    if (0 >= enif_get_string(env, argv[0], text, 256, ERL_NIF_UTF8) ||
        !enif_get_int(env, argv[1], &posX) ||
        !enif_get_int(env, argv[2], &posY) ||
        !enif_get_int(env, argv[3], &fontSize) ||
        !enif_get_tuple_as_color(env, argv[4], &color)) {
	    return enif_make_badarg(env);
    }
    DrawText(text, posX, posY, fontSize, color);
    return enif_make_atom_ok(env);
}

DECLARE_NIF_FUNC(setTargetFPS) {
	int fps;
    if (!enif_get_int(env, argv[0], &fps)) {
	    return enif_make_badarg(env);
    }
    SetTargetFPS(fps);
    return enif_make_atom_ok(env);
}
