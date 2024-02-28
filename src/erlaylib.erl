-module(erlaylib).
-export([
    %%% rcore
    % window-related functions
    initWindow/3,
    closeWindow/0,
    windowShouldClose/0,
    isWindowReady/0,
    isWindowFullscreen/0,
    isWindowHidden/0,
    isWindowMinimized/0,
    isWindowMaximized/0,
    isWindowFocused/0,
    isWindowResized/0,
    isWindowState/1,
    setWindowState/1,
    clearWindowState/1,
    toggleFullscreen/0,
    toggleBorderlessWindowed/0,
    maximizeWindow/0,
    minimizeWindow/0,
    restoreWindow/0,
    setWindowIcon/1,
    setWindowIcons/2,
    setWindowTitle/1,
    setWindowPosition/2,
    setWindowMonitor/1,
    setWindowMinSize/2,
    setWindowMaxSize/2,
    setWindowSize/2,
    setWindowOpacity/1,
    setWindowFocused/0,
    getWindowHandle/0,
    getScreenWidth/0,
    getScreenHeight/0,
    getRenderWidth/0,
    getRenderHeight/0,
    getMonitorCount/0,
    getCurrentMonitor/0,
    getMonitorPosition/1,
    getMonitorWidth/1,
    getMonitorHeight/1,
    getMonitorPhysicalWidth/1,
    getMonitorPhysicalHeight/1,
    getMonitorRefreshRate/1,
    getWindowPosition/0,
    getWindowScaleDPI/0,
    getMonitorName/1,
    setClipboardText/1,
    getClipboardText/0,
    enableEventWaiting/0,
    disableEventWaiting/0,

    beginDrawing/0,
    endDrawing/0,
    clearBackground/1,
    drawText/5,
    setTargetFPS/1
]).
-nifs([
    initWindow/3,
    closeWindow/0,
    windowShouldClose/0,
    isWindowReady/0,
    isWindowFullscreen/0,
    isWindowHidden/0,
    isWindowMinimized/0,
    isWindowMaximized/0,
    isWindowFocused/0,
    isWindowResized/0,
    isWindowState/1,
    setWindowState/1,

    beginDrawing/0,
    endDrawing/0,
    clearBackground/1,
    drawText/5,
    setTargetFPS/1
]).
-on_load(init/0).

init() ->
    erlang:load_nif("priv/erlaylib", 0).

-define(not_loaded, erlang:nif_error("NIF library not loaded")).

initWindow(_Width, _Height, _Title) ->
    ?not_loaded.
closeWindow() ->
    ?not_loaded.
windowShouldClose() ->
    ?not_loaded.
isWindowReady() ->
	?not_loaded.
isWindowFullscreen() ->
	?not_loaded.
isWindowHidden() ->
	?not_loaded.
isWindowMinimized() ->
	?not_loaded.
isWindowMaximized() ->
	?not_loaded.
isWindowFocused() ->
	?not_loaded.
isWindowResized() ->
	?not_loaded.
isWindowState(_Flags) ->
	?not_loaded.
setWindowState(_Flags) ->
	?not_loaded.
clearWindowState(_FLAGS)->
	?not_loaded.
toggleFullscreen()->
	?not_loaded.
toggleBorderlessWindowed()->
	?not_loaded.
maximizeWindow()->
	?not_loaded.
minimizeWindow()->
	?not_loaded.
restoreWindow()->
	?not_loaded.
setWindowIcon(_IMAGE)->
	?not_loaded.
setWindowIcons(_IMAGES,_COUNT)->
	?not_loaded.
setWindowTitle(_TITLE)->
	?not_loaded.
setWindowPosition(_X,_Y)->
	?not_loaded.
setWindowMonitor(_MONITOR)->
	?not_loaded.
setWindowMinSize(_WIDTH, _HEIGHT)->
	?not_loaded.
setWindowMaxSize(_WIDTH, _HEIGHT)->
	?not_loaded.
setWindowSize(_WIDTH, _HEIGHT)->
	?not_loaded.
setWindowOpacity(_OPACITY)->
	?not_loaded.
setWindowFocused()->
	?not_loaded.
getWindowHandle()->
	?not_loaded.
getScreenWidth()->
	?not_loaded.
getScreenHeight()->
	?not_loaded.
getRenderWidth()->
	?not_loaded.
getRenderHeight()->
	?not_loaded.
getMonitorCount()->
	?not_loaded.
getCurrentMonitor()->
	?not_loaded.
vector2GetMonitorPosition(_MONITOR)->
	?not_loaded.
getMonitorWidth(_MONITOR)->
	?not_loaded.
getMonitorHeight(_MONITOR)->
	?not_loaded.
getMonitorPhysicalWidth(_MONITOR)->
	?not_loaded.
getMonitorPhysicalHeight(_MONITOR)->
	?not_loaded.
getMonitorRefreshRate(_MONITOR)->
	?not_loaded.
vector2GetWindowPosition()->
	?not_loaded.
vector2GetWindowScaleDPI()->
	?not_loaded.
getMonitorName(_MONITOR)->
	?not_loaded.
setClipboardText(_TEXT)->
	?not_loaded.
getClipboardText()->
	?not_loaded.
enableEventWaiting()->
	?not_loaded.
disableEventWaiting()->
	?not_loaded.



beginDrawing() ->
    ?not_loaded.
endDrawing() ->
    ?not_loaded.
clearBackground(_Color) ->
    ?not_loaded.
drawText(_Text, _PosX, _PosY, _FontSize, _Color) ->
    ?not_loaded.
setTargetFPS(_FPS) ->
    ?not_loaded.