#define DECLARE_NIF_FUNC(name) ERL_NIF_TERM NIF##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

//
// rcore
//

// Window-related functions
DECLARE_NIF_FUNC(initWindow);
DECLARE_NIF_FUNC(closeWindow);
DECLARE_NIF_FUNC(windowShouldClose);
DECLARE_NIF_FUNC(isWindowReady);
DECLARE_NIF_FUNC(isWindowFullscreen);
DECLARE_NIF_FUNC(isWindowHidden);
DECLARE_NIF_FUNC(isWindowMinimized);
DECLARE_NIF_FUNC(isWindowMaximized);
DECLARE_NIF_FUNC(isWindowFocused);
DECLARE_NIF_FUNC(isWindowResized);
DECLARE_NIF_FUNC(isWindowState);
DECLARE_NIF_FUNC(setWindowState);
DECLARE_NIF_FUNC(ClearWindowState);
DECLARE_NIF_FUNC(ToggleFullscreen);
DECLARE_NIF_FUNC(ToggleBorderlessWindowed);
DECLARE_NIF_FUNC(MaximizeWindow);
DECLARE_NIF_FUNC(MinimizeWindow);
DECLARE_NIF_FUNC(RestoreWindow);
DECLARE_NIF_FUNC(SetWindowIcon);
DECLARE_NIF_FUNC(SetWindowIcons);
DECLARE_NIF_FUNC(SetWindowTitle);
DECLARE_NIF_FUNC(SetWindowPosition);
DECLARE_NIF_FUNC(SetWindowMonitor);
DECLARE_NIF_FUNC(SetWindowMinSize);
DECLARE_NIF_FUNC(SetWindowMaxSize);
DECLARE_NIF_FUNC(SetWindowSize);
DECLARE_NIF_FUNC(SetWindowOpacity);
DECLARE_NIF_FUNC(SetWindowFocused);
DECLARE_NIF_FUNC(GetWindowHandle);
DECLARE_NIF_FUNC(GetScreenWidth);
DECLARE_NIF_FUNC(GetScreenHeight);
DECLARE_NIF_FUNC(GetRenderWidth);
DECLARE_NIF_FUNC(GetRenderHeight);
DECLARE_NIF_FUNC(GetMonitorCount);
DECLARE_NIF_FUNC(GetCurrentMonitor);
DECLARE_NIF_FUNC(GetMonitorPosition);
DECLARE_NIF_FUNC(GetMonitorWidth);
DECLARE_NIF_FUNC(GetMonitorHeight);
DECLARE_NIF_FUNC(GetMonitorPhysicalWidth);
DECLARE_NIF_FUNC(GetMonitorPhysicalHeight);
DECLARE_NIF_FUNC(GetMonitorRefreshRate);
DECLARE_NIF_FUNC(GetWindowPosition);
DECLARE_NIF_FUNC(GetWindowScaleDPI);
DECLARE_NIF_FUNC(GetMonitorName);
DECLARE_NIF_FUNC(SetClipboardText);
DECLARE_NIF_FUNC(GetClipboardText);
DECLARE_NIF_FUNC(EnableEventWaiting);
DECLARE_NIF_FUNC(DisableEventWaiting);

DECLARE_NIF_FUNC(beginDrawing);
DECLARE_NIF_FUNC(endDrawing);
DECLARE_NIF_FUNC(clearBackground);
DECLARE_NIF_FUNC(drawText);
DECLARE_NIF_FUNC(setTargetFPS);