#include "erl_nif.h"
#include "raylib.h"

#include "nifhelpers.h"

#include "nif_funcs.h"

DECLARE_NIF_FUNC(initWindow) {
    int width;
    int height;
    char title[256];
    if (!enif_get_int(env, argv[0], &width) ||
        !enif_get_int(env, argv[1], &height) ||
        0 >= enif_get_string(env, argv[2], title, 256, ERL_NIF_UTF8)) {
	    return enif_make_badarg(env);
    }
    InitWindow(width, height, title);
    return enif_make_atom_ok(env);
}

DECLARE_NIF_FUNC(closeWindow) {
    CloseWindow();
    return enif_make_atom_ok(env);
}

DECLARE_NIF_FUNC(windowShouldClose) {
    return enif_make_atom_from_bool(env, WindowShouldClose());
}

DECLARE_NIF_FUNC(isWindowReady) {
    return enif_make_atom_from_bool(env, IsWindowReady());
}

DECLARE_NIF_FUNC(isWindowFullscreen) {
    return enif_make_atom_from_bool(env, IsWindowFullscreen());
}

DECLARE_NIF_FUNC(isWindowHidden) {
    return enif_make_atom_from_bool(env, IsWindowHidden());
}

DECLARE_NIF_FUNC(isWindowMinimized) {
    return enif_make_atom_from_bool(env, IsWindowMinimized());
}

DECLARE_NIF_FUNC(isWindowMaximized) {
    return enif_make_atom_from_bool(env, IsWindowMaximized());
}
DECLARE_NIF_FUNC(isWindowFocused) {
    return enif_make_atom_from_bool(env, IsWindowFocused());
}

DECLARE_NIF_FUNC(isWindowResized) {
    return enif_make_atom_from_bool(env, IsWindowResized());
}

DECLARE_NIF_FUNC(isWindowState) {
    unsigned int flags;
    if (!enif_get_uint(env, argv[0], &flags)) {
	    return enif_make_badarg(env);
    }
    return enif_make_atom_from_bool(env, IsWindowState(flags));
}

DECLARE_NIF_FUNC(setWindowState) {
    unsigned int flags;
    if (!enif_get_uint(env, argv[0], &flags)) {
	    return enif_make_badarg(env);
    }
    SetWindowState(flags);
    return enif_make_atom_ok(env);
}
