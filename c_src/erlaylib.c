#include "erl_nif.h"
#include "raylib.h"

#include "nif_funcs.h"
#include "nifhelpers.h"

#define NIF_FUNC(name, arity, flag) {#name, arity, NIF##name, flag}

static ErlNifFunc nif_funcs[] = {
    // rcore
    NIF_FUNC(initWindow, 3, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(closeWindow, 0, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(windowShouldClose, 0, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(isWindowReady, 0,),
    NIF_FUNC(isWindowFullscreen, 0,),
    NIF_FUNC(isWindowHidden, 0,),
    NIF_FUNC(isWindowMinimized, 0,),
    NIF_FUNC(isWindowMaximized, 0,),
    NIF_FUNC(isWindowFocused, 0,),
    NIF_FUNC(isWindowResized, 0,),
    NIF_FUNC(isWindowState, 1,),
    NIF_FUNC(setWindowState, 1,),
    NIF_FUNC(clearWindowState, 1,),
    NIF_FUNC(toggleFullscreen, 0,),
    NIF_FUNC(toggleBorderlessWindowed, 0,),
    NIF_FUNC(maximizeWindow, 0,),
    NIF_FUNC(minimizeWindow, 0,),
    NIF_FUNC(restoreWindow, 0,),
    NIF_FUNC(setWindowIcon, 1,),
    NIF_FUNC(setWindowIcons, 2,),
    NIF_FUNC(setWindowTitle, 1,),
    NIF_FUNC(setWindowPosition, 2,),
    NIF_FUNC(setWindowMonitor, 1,),
    NIF_FUNC(setWindowMinSize, 2,),
    NIF_FUNC(setWindowMaxSize, 2,),
    NIF_FUNC(setWindowSize, 2,),
    NIF_FUNC(setWindowOpacity, 1,),
    NIF_FUNC(setWindowFocused, 0,),
    NIF_FUNC(getWindowHandle, 0,),
    NIF_FUNC(getScreenWidth, 0,),
    NIF_FUNC(getScreenHeight, 0,),
    NIF_FUNC(getRenderWidth, 0,),
    NIF_FUNC(getRenderHeight, 0,),
    NIF_FUNC(getMonitorCount, 0,),
    NIF_FUNC(getCurrentMonitor, 0,),
    NIF_FUNC(getMonitorPosition, 1,),
    NIF_FUNC(getMonitorWidth, 1,),
    NIF_FUNC(getMonitorHeight, 1,),
    NIF_FUNC(getMonitorPhysicalWidth, 1,),
    NIF_FUNC(getMonitorPhysicalHeight, 1,),
    NIF_FUNC(getMonitorRefreshRate, 1,),
    NIF_FUNC(getWindowPosition, 0,),
    NIF_FUNC(getWindowScaleDPI, 0,),
    NIF_FUNC(getMonitorName, 1,),
    NIF_FUNC(setClipboardText, 1,),
    NIF_FUNC(getClipboardText, 0,),
    NIF_FUNC(enableEventWaiting, 0,),
    NIF_FUNC(disableEventWaiting, 0,),

    // other
    NIF_FUNC(beginDrawing, 0, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(endDrawing, 0, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(clearBackground, 1, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(drawText, 5, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(setTargetFPS, 1,),
};

ERL_NIF_INIT(erlaylib, nif_funcs, NULL, NULL, NULL, NULL);