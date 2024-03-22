#include "erl_nif.h"
#include "raylib.h"

#include "nif_funcs.h"
#include "nif_helpers.h"

#define NIF_FUNC(name, arity, flag)   \
    {                                 \
        #name, arity, NIF##name, flag \
    }

static ErlNifFunc nif_funcs[] = {
    /// -----------------------------------------------------------------------------------
    ///
    /// module: rcore

    // Window-related functions
    NIF_FUNC(initWindow, 3, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(closeWindow, 0, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(windowShouldClose, 0, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(isWindowReady, 0, ),
    NIF_FUNC(isWindowFullscreen, 0, ),
    NIF_FUNC(isWindowHidden, 0, ),
    NIF_FUNC(isWindowMinimized, 0, ),
    NIF_FUNC(isWindowMaximized, 0, ),
    NIF_FUNC(isWindowFocused, 0, ),
    NIF_FUNC(isWindowResized, 0, ),
    NIF_FUNC(isWindowState, 1, ),
    NIF_FUNC(setWindowState, 1, ),
    NIF_FUNC(clearWindowState, 1, ),
    NIF_FUNC(toggleFullscreen, 0, ),
    NIF_FUNC(toggleBorderlessWindowed, 0, ),
    NIF_FUNC(maximizeWindow, 0, ),
    NIF_FUNC(minimizeWindow, 0, ),
    NIF_FUNC(restoreWindow, 0, ),
    NIF_FUNC(setWindowIcon, 1, ),
    NIF_FUNC(setWindowIcons, 2, ),
    NIF_FUNC(setWindowTitle, 1, ),
    NIF_FUNC(setWindowPosition, 2, ),
    NIF_FUNC(setWindowMonitor, 1, ),
    NIF_FUNC(setWindowMinSize, 2, ),
    NIF_FUNC(setWindowMaxSize, 2, ),
    NIF_FUNC(setWindowSize, 2, ),
    NIF_FUNC(setWindowOpacity, 1, ),
    NIF_FUNC(setWindowFocused, 0, ),
    NIF_FUNC(getWindowHandle, 0, ),
    NIF_FUNC(getScreenWidth, 0, ),
    NIF_FUNC(getScreenHeight, 0, ),
    NIF_FUNC(getRenderWidth, 0, ),
    NIF_FUNC(getRenderHeight, 0, ),
    NIF_FUNC(getMonitorCount, 0, ),
    NIF_FUNC(getCurrentMonitor, 0, ),
    NIF_FUNC(getMonitorPosition, 1, ),
    NIF_FUNC(getMonitorWidth, 1, ),
    NIF_FUNC(getMonitorHeight, 1, ),
    NIF_FUNC(getMonitorPhysicalWidth, 1, ),
    NIF_FUNC(getMonitorPhysicalHeight, 1, ),
    NIF_FUNC(getMonitorRefreshRate, 1, ),
    NIF_FUNC(getWindowPosition, 0, ),
    NIF_FUNC(getWindowScaleDPI, 0, ),
    NIF_FUNC(getMonitorName, 1, ),
    NIF_FUNC(setClipboardText, 1, ),
    NIF_FUNC(getClipboardText, 0, ),
    NIF_FUNC(enableEventWaiting, 0, ),
    NIF_FUNC(disableEventWaiting, 0, ),

    // Cursor-related functions
    NIF_FUNC(showCursor, 0, ),
    NIF_FUNC(hideCursor, 0, ),
    NIF_FUNC(isCursorHidden, 0, ),
    NIF_FUNC(enableCursor, 0, ),
    NIF_FUNC(disableCursor, 0, ),
    NIF_FUNC(isCursorOnScreen, 0, ),

    // Drawing-related functions
    NIF_FUNC(clearBackground, 1, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(beginDrawing, 0, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(endDrawing, 0, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(beginMode2D, 1, ),
    NIF_FUNC(endMode2D, 0, ),
    NIF_FUNC(beginMode3D, 1, ),
    NIF_FUNC(endMode3D, 0, ),
    NIF_FUNC(beginTextureMode, 1, ),
    NIF_FUNC(endTextureMode, 0, ),
    NIF_FUNC(beginShaderMode, 1, ),
    NIF_FUNC(endShaderMode, 0, ),
    NIF_FUNC(beginBlendMode, 1, ),
    NIF_FUNC(endBlendMode, 0, ),
    NIF_FUNC(beginScissorMode, 4, ),
    NIF_FUNC(endScissorMode, 0, ),
    NIF_FUNC(beginVrStereoMode, 1, ),
    NIF_FUNC(endVrStereoMode, 0, ),

    // VR stereo config functions for VR simulator
    NIF_FUNC(loadVrStereoConfig, 1, ),
    NIF_FUNC(unloadVrStereoConfig, 1, ),

    // Shader management functions
    // NOTE: Shader functionality is not available on OpenGL 1.1
    NIF_FUNC(loadShader, 2, ),
    NIF_FUNC(loadShaderFromMemory, 2, ),
    NIF_FUNC(isShaderReady, 1, ),
    NIF_FUNC(getShaderLocation, 2, ),
    NIF_FUNC(getShaderLocationAttrib, 2, ),
    NIF_FUNC(setShaderValue, 4, ),
    NIF_FUNC(setShaderValueV, 5, ),
    NIF_FUNC(setShaderValueMatrix, 3, ),
    NIF_FUNC(setShaderValueTexture, 3, ),
    NIF_FUNC(unloadShader, 1, ),

    // Screen-space-related functions
    NIF_FUNC(getMouseRay, 2, ),
    NIF_FUNC(getCameraMatrix, 1, ),
    NIF_FUNC(getCameraMatrix2D, 1, ),
    NIF_FUNC(getWorldToScreen, 2, ),
    NIF_FUNC(getScreenToWorld2D, 2, ),
    NIF_FUNC(getWorldToScreenEx, 4, ),
    NIF_FUNC(getWorldToScreen2D, 2, ),

    // Timing-related functions
    NIF_FUNC(setTargetFPS, 1, ),
    NIF_FUNC(getFrameTime, 0, ),
    NIF_FUNC(getTime, 0, ),
    NIF_FUNC(getFPS, 0, ),

    // Custom frame control functions
    // NOTE: Those functions are intended for advance users that want full control over the frame processing
    // By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timing + PollInputEvents()
    // To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
    NIF_FUNC(swapScreenBuffer, 0, ),
    NIF_FUNC(pollInputEvents, 0, ),
    NIF_FUNC(waitTime, 1, ),

    // Random values generation functions
    NIF_FUNC(setRandomSeed, 1, ),
    NIF_FUNC(getRandomValue, 2, ),
    NIF_FUNC(loadRandomSequence, 3, ),
    NIF_FUNC(unloadRandomSequence, 1, ),

    // Misc. functions
    NIF_FUNC(takeScreenshot, 1, ),
    NIF_FUNC(setConfigFlags, 1, ),
    NIF_FUNC(openURL, 1, ),

    // NOTE: Following functions implemented in module [utils]
    //------------------------------------------------------------------
    NIF_FUNC(traceLog, 3, ),
    NIF_FUNC(setTraceLogLevel, 1, ),
    NIF_FUNC(memAlloc, 1, ),
    NIF_FUNC(memRealloc, 2, ),
    NIF_FUNC(memFree, 1, ),

    // Set custom callbacks
    // WARNING: Callbacks setup is intended for advance users
    NIF_FUNC(setTraceLogCallback, 1, ),
    NIF_FUNC(setLoadFileDataCallback, 1, ),
    NIF_FUNC(setSaveFileDataCallback, 1, ),
    NIF_FUNC(setLoadFileTextCallback, 1, ),
    NIF_FUNC(setSaveFileTextCallback, 1, ),

    // Files management functions
    NIF_FUNC(loadFileData, 2, ),
    NIF_FUNC(unloadFileData, 1, ),
    NIF_FUNC(saveFileData, 3, ),
    NIF_FUNC(exportDataAsCode, 3, ),
    NIF_FUNC(loadFileText, 1, ),
    NIF_FUNC(unloadFileText, 1, ),
    NIF_FUNC(saveFileText, 2, ),
    //------------------------------------------------------------------

    // File system functions
    NIF_FUNC(fileExists, 1, ),
    NIF_FUNC(directoryExists, 1, ),
    NIF_FUNC(isFileExtension, 2, ),
    NIF_FUNC(getFileLength, 1, ),
    NIF_FUNC(getFileExtension, 1, ),
    NIF_FUNC(getFileName, 1, ),
    NIF_FUNC(getFileNameWithoutExt, 1, ),
    NIF_FUNC(getDirectoryPath, 1, ),
    NIF_FUNC(getPrevDirectoryPath, 1, ),
    NIF_FUNC(getWorkingDirectory, 0, ),
    NIF_FUNC(getApplicationDirectory, 0, ),
    NIF_FUNC(changeDirectory, 1, ),
    NIF_FUNC(isPathFile, 1, ),
    NIF_FUNC(loadDirectoryFiles, 1, ),
    NIF_FUNC(loadDirectoryFilesEx, 3, ),
    NIF_FUNC(unloadDirectoryFiles, 1, ),
    NIF_FUNC(isFileDropped, 0, ),
    NIF_FUNC(loadDroppedFiles, 0, ),
    NIF_FUNC(unloadDroppedFiles, 1, ),
    NIF_FUNC(getFileModTime, 1, ),

    // Compression/Encoding functionality
    NIF_FUNC(compressData, 3, ),
    NIF_FUNC(decompressData, 3, ),
    NIF_FUNC(encodeDataBase64, 3, ),
    NIF_FUNC(decodeDataBase64, 2, ),

    // Automation events functionality
    NIF_FUNC(loadAutomationEventList, 1, ),
    NIF_FUNC(unloadAutomationEventList, 1, ),
    NIF_FUNC(exportAutomationEventList, 2, ),
    NIF_FUNC(setAutomationEventList, 1, ),
    NIF_FUNC(setAutomationEventBaseFrame, 1, ),
    NIF_FUNC(startAutomationEventRecording, 0, ),
    NIF_FUNC(stopAutomationEventRecording, 0, ),
    NIF_FUNC(playAutomationEvent, 1, ),

    //------------------------------------------------------------------------------------
    // Input Handling Functions (Module: core)
    //------------------------------------------------------------------------------------

    // Input-related functions: keyboard
    NIF_FUNC(isKeyPressed, 1, ),
    NIF_FUNC(isKeyPressedRepeat, 1, ),
    NIF_FUNC(isKeyDown, 1, ),
    NIF_FUNC(isKeyReleased, 1, ),
    NIF_FUNC(isKeyUp, 1, ),
    NIF_FUNC(getKeyPressed, 0, ),
    NIF_FUNC(getCharPressed, 0, ),
    NIF_FUNC(setExitKey, 1, ),

    // Input-related functions: gamepads
    NIF_FUNC(isGamepadAvailable, 1, ),
    NIF_FUNC(getGamepadName, 1, ),
    NIF_FUNC(isGamepadButtonPressed, 2, ),
    NIF_FUNC(isGamepadButtonDown, 2, ),
    NIF_FUNC(isGamepadButtonReleased, 2, ),
    NIF_FUNC(isGamepadButtonUp, 2, ),
    NIF_FUNC(getGamepadButtonPressed, 0, ),
    NIF_FUNC(getGamepadAxisCount, 1, ),
    NIF_FUNC(getGamepadAxisMovement, 2, ),
    NIF_FUNC(setGamepadMappings, 1, ),

    // Input-related functions: mouse
    NIF_FUNC(isMouseButtonPressed, 1, ),
    NIF_FUNC(isMouseButtonDown, 1, ),
    NIF_FUNC(isMouseButtonReleased, 1, ),
    NIF_FUNC(isMouseButtonUp, 1, ),
    NIF_FUNC(getMouseX, 0, ),
    NIF_FUNC(getMouseY, 0, ),
    NIF_FUNC(getMousePosition, 0, ),
    NIF_FUNC(getMouseDelta, 0, ),
    NIF_FUNC(setMousePosition, 2, ),
    NIF_FUNC(setMouseOffset, 2, ),
    NIF_FUNC(setMouseScale, 2, ),
    NIF_FUNC(getMouseWheelMove, 0, ),
    NIF_FUNC(getMouseWheelMoveV, 0, ),
    NIF_FUNC(setMouseCursor, 1, ),

    // Input-related functions: touch
    NIF_FUNC(getTouchX, 0, ),
    NIF_FUNC(getTouchY, 0, ),
    NIF_FUNC(getTouchPosition, 1, ),
    NIF_FUNC(getTouchPointId, 1, ),
    NIF_FUNC(getTouchPointCount, 0, ),

    //------------------------------------------------------------------------------------
    // Gestures and Touch Handling Functions (Module: rgestures)
    //------------------------------------------------------------------------------------
    NIF_FUNC(setGesturesEnabled, 1, ),
    NIF_FUNC(isGestureDetected, 1, ),
    NIF_FUNC(getGestureDetected, 0, ),
    NIF_FUNC(getGestureHoldDuration, 0, ),
    NIF_FUNC(getGestureDragVector, 0, ),
    NIF_FUNC(getGestureDragAngle, 0, ),
    NIF_FUNC(getGesturePinchVector, 0, ),
    NIF_FUNC(getGesturePinchAngle, 0, ),

    //------------------------------------------------------------------------------------
    // Camera System Functions (Module: rcamera)
    //------------------------------------------------------------------------------------
    NIF_FUNC(updateCamera, 2, ),
    NIF_FUNC(updateCameraPro, 4, ),

    /// -----------------------------------------------------------------------------------
    ///
    /// module: rshapes

    // NOTE: It can be useful when using basic shapes and one single font,
    // defining a font char white rectangle would allow drawing everything in a single draw call
    NIF_FUNC(setShapesTexture, 2, ),

    // Basic shapes drawing functions
    NIF_FUNC(drawPixel, 3, ),
    NIF_FUNC(drawPixelV, 2, ),
    NIF_FUNC(drawLine, 5, ),
    NIF_FUNC(drawLineV, 3, ),
    NIF_FUNC(drawLineEx, 4, ),
    NIF_FUNC(drawLineStrip, 3, ),
    NIF_FUNC(drawLineBezier, 4, ),
    NIF_FUNC(drawCircle, 4, ),
    NIF_FUNC(drawCircleSector, 6, ),
    NIF_FUNC(drawCircleSectorLines, 6, ),
    NIF_FUNC(drawCircleGradient, 5, ),
    NIF_FUNC(drawCircleV, 3, ),
    NIF_FUNC(drawCircleLines, 4, ),
    NIF_FUNC(drawCircleLinesV, 3, ),
    NIF_FUNC(drawEllipse, 5, ),
    NIF_FUNC(drawEllipseLines, 5, ),
    NIF_FUNC(drawRing, 7, ),
    NIF_FUNC(drawRingLines, 7, ),
    NIF_FUNC(drawRectangle, 5, ),
    NIF_FUNC(drawRectangleV, 3, ),
    NIF_FUNC(drawRectangleRec, 2, ),
    NIF_FUNC(drawRectanglePro, 4, ),
    NIF_FUNC(drawRectangleGradientV, 6, ),
    NIF_FUNC(drawRectangleGradientH, 6, ),
    NIF_FUNC(drawRectangleGradientEx, 5, ),
    NIF_FUNC(drawRectangleLines, 5, ),
    NIF_FUNC(drawRectangleLinesEx, 3, ),
    NIF_FUNC(drawRectangleRounded, 4, ),
    NIF_FUNC(drawRectangleRoundedLines, 5, ),
    NIF_FUNC(drawTriangle, 4, ),
    NIF_FUNC(drawTriangleLines, 4, ),
    NIF_FUNC(drawTriangleFan, 3, ),
    NIF_FUNC(drawTriangleStrip, 3, ),
    NIF_FUNC(drawPoly, 5, ),
    NIF_FUNC(drawPolyLines, 5, ),
    NIF_FUNC(drawPolyLinesEx, 6, ),

    // Splines drawing functions
    NIF_FUNC(drawSplineLinear, 4, ),
    NIF_FUNC(drawSplineBasis, 4, ),
    NIF_FUNC(drawSplineCatmullRom, 4, ),
    NIF_FUNC(drawSplineBezierQuadratic, 4, ),
    NIF_FUNC(drawSplineBezierCubic, 4, ),
    NIF_FUNC(drawSplineSegmentLinear, 4, ),
    NIF_FUNC(drawSplineSegmentBasis, 6, ),
    NIF_FUNC(drawSplineSegmentCatmullRom, 6, ),
    NIF_FUNC(drawSplineSegmentBezierQuadratic, 5, ),
    NIF_FUNC(drawSplineSegmentBezierCubic, 6, ),

    // Spline segment point evaluation functions, for a given t [0.0f .. 1.0f]
    NIF_FUNC(getSplinePointLinear, 3, ),
    NIF_FUNC(getSplinePointBasis, 5, ),
    NIF_FUNC(getSplinePointCatmullRom, 5, ),
    NIF_FUNC(getSplinePointBezierQuad, 4, ),
    NIF_FUNC(getSplinePointBezierCubic, 5, ),

    // Basic shapes collision detection functions
    NIF_FUNC(checkCollisionRecs, 2, ),
    NIF_FUNC(checkCollisionCircles, 4, ),
    NIF_FUNC(checkCollisionCircleRec, 3, ),
    NIF_FUNC(checkCollisionPointRec, 2, ),
    NIF_FUNC(checkCollisionPointCircle, 3, ),
    NIF_FUNC(checkCollisionPointTriangle, 4, ),
    NIF_FUNC(checkCollisionPointPoly, 3, ),
    NIF_FUNC(checkCollisionLines, 5, ),
    NIF_FUNC(checkCollisionPointLine, 4, ),
    NIF_FUNC(getCollisionRec, 2, ),

    /// -----------------------------------------------------------------------------------
    ///
    /// module: rtextures

    // Image loading functions
    // NOTE: These functions do not require GPU access
    NIF_FUNC(loadImage, 1, ),
    NIF_FUNC(loadImageRaw, 5, ),
    NIF_FUNC(loadImageSvg, 3, ),
    NIF_FUNC(loadImageAnim, 2, ),
    NIF_FUNC(loadImageFromMemory, 3, ),
    NIF_FUNC(loadImageFromTexture, 1, ),
    NIF_FUNC(loadImageFromScreen, 0, ),
    NIF_FUNC(isImageReady, 1, ),
    NIF_FUNC(unloadImage, 1, ),
    NIF_FUNC(exportImage, 2, ),
    NIF_FUNC(exportImageToMemory, 3, ),
    NIF_FUNC(exportImageAsCode, 2, ),

    // Image generation functions
    NIF_FUNC(genImageColor, 3, ),
    NIF_FUNC(genImageGradientLinear, 5, ),
    NIF_FUNC(genImageGradientRadial, 5, ),
    NIF_FUNC(genImageGradientSquare, 5, ),
    NIF_FUNC(genImageChecked, 6, ),
    NIF_FUNC(genImageWhiteNoise, 3, ),
    NIF_FUNC(genImagePerlinNoise, 5, ),
    NIF_FUNC(genImageCellular, 3, ),
    NIF_FUNC(genImageText, 3, ),

    // Image manipulation functions
    NIF_FUNC(imageCopy, 1, ),
    NIF_FUNC(imageFromImage, 2, ),
    NIF_FUNC(imageText, 3, ),
    NIF_FUNC(imageTextEx, 5, ),
    NIF_FUNC(imageFormat, 2, ),
    NIF_FUNC(imageToPOT, 2, ),
    NIF_FUNC(imageCrop, 2, ),
    NIF_FUNC(imageAlphaCrop, 2, ),
    NIF_FUNC(imageAlphaClear, 3, ),
    NIF_FUNC(imageAlphaMask, 2, ),
    NIF_FUNC(imageAlphaPremultiply, 1, ),
    NIF_FUNC(imageBlurGaussian, 2, ),
    NIF_FUNC(imageResize, 3, ),
    NIF_FUNC(imageResizeNN, 3, ),
    NIF_FUNC(imageResizeCanvas, 6, ),
    NIF_FUNC(imageMipmaps, 1, ),
    NIF_FUNC(imageDither, 5, ),
    NIF_FUNC(imageFlipVertical, 1, ),
    NIF_FUNC(imageFlipHorizontal, 1, ),
    NIF_FUNC(imageRotate, 2, ),
    NIF_FUNC(imageRotateCW, 1, ),
    NIF_FUNC(imageRotateCCW, 1, ),
    NIF_FUNC(imageColorTint, 2, ),
    NIF_FUNC(imageColorInvert, 1, ),
    NIF_FUNC(imageColorGrayscale, 1, ),
    NIF_FUNC(imageColorContrast, 2, ),
    NIF_FUNC(imageColorBrightness, 2, ),
    NIF_FUNC(imageColorReplace, 3, ),
    NIF_FUNC(loadImageColors, 1, ),
    NIF_FUNC(loadImagePalette, 3, ),
    NIF_FUNC(unloadImageColors, 1, ),
    NIF_FUNC(unloadImagePalette, 1, ),
    NIF_FUNC(getImageAlphaBorder, 2, ),
    NIF_FUNC(getImageColor, 3, ),

    // Image drawing functions
    // NOTE: Image software-rendering functions (CPU)
    NIF_FUNC(imageClearBackground, 2, ),
    NIF_FUNC(imageDrawPixel, 4, ),
    NIF_FUNC(imageDrawPixelV, 3, ),
    NIF_FUNC(imageDrawLine, 6, ),
    NIF_FUNC(imageDrawLineV, 4, ),
    NIF_FUNC(imageDrawCircle, 5, ),
    NIF_FUNC(imageDrawCircleV, 4, ),
    NIF_FUNC(imageDrawCircleLines, 5, ),
    NIF_FUNC(imageDrawCircleLinesV, 4, ),
    NIF_FUNC(imageDrawRectangle, 6, ),
    NIF_FUNC(imageDrawRectangleV, 4, ),
    NIF_FUNC(imageDrawRectangleRec, 3, ),
    NIF_FUNC(imageDrawRectangleLines, 4, ),
    NIF_FUNC(imageDraw, 5, ),
    NIF_FUNC(imageDrawText, 6, ),
    NIF_FUNC(imageDrawTextEx, 7, ),

    // Texture loading functions
    // NOTE: These functions require GPU access
    NIF_FUNC(loadTexture, 1, ),
    NIF_FUNC(loadTextureFromImage, 1, ),
    NIF_FUNC(loadTextureCubemap, 2, ),
    NIF_FUNC(loadRenderTexture, 2, ),
    NIF_FUNC(isTextureReady, 1, ),
    NIF_FUNC(unloadTexture, 1, ),
    NIF_FUNC(isRenderTextureReady, 1, ),
    NIF_FUNC(unloadRenderTexture, 1, ),
    NIF_FUNC(updateTexture, 2, ),
    NIF_FUNC(updateTextureRec, 3, ),

    // Texture configuration functions
    NIF_FUNC(genTextureMipmaps, 1, ),
    NIF_FUNC(setTextureFilter, 2, ),
    NIF_FUNC(setTextureWrap, 2, ),

    // Texture drawing functions
    NIF_FUNC(drawTexture, 4, ),
    NIF_FUNC(drawTextureV, 3, ),
    NIF_FUNC(drawTextureEx, 5, ),
    NIF_FUNC(drawTextureRec, 4, ),
    NIF_FUNC(drawTexturePro, 6, ),
    NIF_FUNC(drawTextureNPatch, 6, ),

    // Color/pixel related functions
    NIF_FUNC(fade, 2, ),
    NIF_FUNC(colorToInt, 1, ),
    NIF_FUNC(colorNormalize, 1, ),
    NIF_FUNC(colorFromNormalized, 1, ),
    NIF_FUNC(colorToHSV, 1, ),
    NIF_FUNC(colorFromHSV, 3, ),
    NIF_FUNC(colorTint, 2, ),
    NIF_FUNC(colorBrightness, 2, ),
    NIF_FUNC(colorContrast, 2, ),
    NIF_FUNC(colorAlpha, 2, ),
    NIF_FUNC(colorAlphaBlend, 3, ),
    NIF_FUNC(getColor, 1, ),
    NIF_FUNC(getPixelColor, 2, ),
    NIF_FUNC(setPixelColor, 3, ),
    NIF_FUNC(getPixelDataSize, 3, ),

    /// -----------------------------------------------------------------------------------
    ///
    /// module: rtext

    // Font loading/unloading functions
    NIF_FUNC(getFontDefault, 0, ),
    NIF_FUNC(loadFont, 1, ),
    NIF_FUNC(loadFontEx, 4, ),
    NIF_FUNC(loadFontFromImage, 3, ),
    NIF_FUNC(loadFontFromMemory, 6, ),
    NIF_FUNC(isFontReady, 1, ),
    NIF_FUNC(loadFontData, 6, ),
    NIF_FUNC(genImageFontAtlas, 6, ),
    NIF_FUNC(unloadFontData, 2, ),
    NIF_FUNC(unloadFont, 1, ),
    NIF_FUNC(exportFontAsCode, 2, ),

    // Text drawing functions
    NIF_FUNC(drawFPS, 2, ),
    NIF_FUNC(drawText, 5, ERL_NIF_DIRTY_JOB_IO_BOUND),
    NIF_FUNC(drawTextEx, 6, ),
    NIF_FUNC(drawTextPro, 8, ),
    NIF_FUNC(drawTextCodepoint, 5, ),
    NIF_FUNC(drawTextCodepoints, 7, ),

    // Text font info functions
    NIF_FUNC(setTextLineSpacing, 1, ),
    NIF_FUNC(measureText, 2, ),
    NIF_FUNC(measureTextEx, 4, ),
    NIF_FUNC(getGlyphIndex, 2, ),
    NIF_FUNC(getGlyphInfo, 2, ),
    NIF_FUNC(getGlyphAtlasRec, 2, ),

    // Text codepoints management functions (unicode characters)
    NIF_FUNC(loadUTF8, 2, ),
    NIF_FUNC(unloadUTF8, 1, ),
    NIF_FUNC(loadCodepoints, 2, ),
    NIF_FUNC(unloadCodepoints, 1, ),
    NIF_FUNC(getCodepointCount, 1, ),
    NIF_FUNC(getCodepoint, 2, ),
    NIF_FUNC(getCodepointNext, 2, ),
    NIF_FUNC(getCodepointPrevious, 2, ),
    NIF_FUNC(codepointToUTF8, 2, ),

    // Text strings management functions (no UTF-8 strings, only byte chars)
    // NOTE: Some strings allocate memory internally for returned strings, just be careful!
    NIF_FUNC(textCopy, 2, ),
    NIF_FUNC(textIsEqual, 2, ),
    NIF_FUNC(textLength, 1, ),
    NIF_FUNC(textFormat, 2, ),
    NIF_FUNC(textSubtext, 3, ),
    NIF_FUNC(textReplace, 3, ),
    NIF_FUNC(textInsert, 3, ),
    NIF_FUNC(textJoin, 3, ),
    NIF_FUNC(textSplit, 3, ),
    NIF_FUNC(textAppend, 3, ),
    NIF_FUNC(textFindIndex, 2, ),
    NIF_FUNC(textToUpper, 1, ),
    NIF_FUNC(textToLower, 1, ),
    NIF_FUNC(textToPascal, 1, ),
    NIF_FUNC(textToInteger, 1, ),

    /// -----------------------------------------------------------------------------------
    ///
    /// module: rmodels

    // Basic geometric 3D shapes drawing functions
    NIF_FUNC(drawLine3D, 3, ),
    NIF_FUNC(drawPoint3D, 2, ),
    NIF_FUNC(drawCircle3D, 5, ),
    NIF_FUNC(drawTriangle3D, 4, ),
    NIF_FUNC(drawTriangleStrip3D, 3, ),
    NIF_FUNC(drawCube, 5, ),
    NIF_FUNC(drawCubeV, 3, ),
    NIF_FUNC(drawCubeWires, 5, ),
    NIF_FUNC(drawCubeWiresV, 3, ),
    NIF_FUNC(drawSphere, 3, ),
    NIF_FUNC(drawSphereEx, 5, ),
    NIF_FUNC(drawSphereWires, 5, ),
    NIF_FUNC(drawCylinder, 6, ),
    NIF_FUNC(drawCylinderEx, 6, ),
    NIF_FUNC(drawCylinderWires, 6, ),
    NIF_FUNC(drawCylinderWiresEx, 6, ),
    NIF_FUNC(drawCapsule, 6, ),
    NIF_FUNC(drawCapsuleWires, 6, ),
    NIF_FUNC(drawPlane, 3, ),
    NIF_FUNC(drawRay, 2, ),
    NIF_FUNC(drawGrid, 2, ),

    // Model management functions
    NIF_FUNC(loadModel, 1, ),
    NIF_FUNC(loadModelFromMesh, 1, ),
    NIF_FUNC(isModelReady, 1, ),
    NIF_FUNC(unloadModel, 1, ),
    NIF_FUNC(getModelBoundingBox, 1, ),

    // Model drawing functions
    NIF_FUNC(drawModel, 4, ),
    NIF_FUNC(drawModelEx, 6, ),
    NIF_FUNC(drawModelWires, 4, ),
    NIF_FUNC(drawModelWiresEx, 6, ),
    NIF_FUNC(drawBoundingBox, 2, ),
    NIF_FUNC(drawBillboard, 5, ),
    NIF_FUNC(drawBillboardRec, 6, ),
    NIF_FUNC(drawBillboardPro, 9, ),

    // Mesh management functions
    NIF_FUNC(uploadMesh, 2, ),
    NIF_FUNC(updateMeshBuffer, 5, ),
    NIF_FUNC(unloadMesh, 1, ),
    NIF_FUNC(drawMesh, 3, ),
    NIF_FUNC(drawMeshInstanced, 4, ),
    NIF_FUNC(exportMesh, 2, ),
    NIF_FUNC(getMeshBoundingBox, 1, ),
    NIF_FUNC(genMeshTangents, 1, ),

    // Mesh generation functions
    NIF_FUNC(genMeshPoly, 2, ),
    NIF_FUNC(genMeshPlane, 4, ),
    NIF_FUNC(genMeshCube, 3, ),
    NIF_FUNC(genMeshSphere, 3, ),
    NIF_FUNC(genMeshHemiSphere, 3, ),
    NIF_FUNC(genMeshCylinder, 3, ),
    NIF_FUNC(genMeshCone, 3, ),
    NIF_FUNC(genMeshTorus, 4, ),
    NIF_FUNC(genMeshKnot, 4, ),
    NIF_FUNC(genMeshHeightmap, 2, ),
    NIF_FUNC(genMeshCubicmap, 2, ),

    // Material loading/unloading functions
    NIF_FUNC(loadMaterials, 2, ),
    NIF_FUNC(loadMaterialDefault, 0, ),
    NIF_FUNC(isMaterialReady, 1, ),
    NIF_FUNC(unloadMaterial, 1, ),
    NIF_FUNC(setMaterialTexture, 3, ),
    NIF_FUNC(setModelMeshMaterial, 3, ),

    // Model animations loading/unloading functions
    NIF_FUNC(loadModelAnimations, 2, ),
    NIF_FUNC(updateModelAnimation, 3, ),
    NIF_FUNC(unloadModelAnimation, 1, ),
    NIF_FUNC(unloadModelAnimations, 2, ),
    NIF_FUNC(isModelAnimationValid, 2, ),

    // Collision detection functions
    NIF_FUNC(checkCollisionSpheres, 4, ),
    NIF_FUNC(checkCollisionBoxes, 2, ),
    NIF_FUNC(checkCollisionBoxSphere, 3, ),
    NIF_FUNC(getRayCollisionSphere, 3, ),
    NIF_FUNC(getRayCollisionBox, 2, ),
    NIF_FUNC(getRayCollisionMesh, 3, ),
    NIF_FUNC(getRayCollisionTriangle, 4, ),
    NIF_FUNC(getRayCollisionQuad, 5, ),

    /// -----------------------------------------------------------------------------------
    ///
    /// module: raudio

    // Audio device management functions
    NIF_FUNC(initAudioDevice, 0, ),
    NIF_FUNC(closeAudioDevice, 0, ),
    NIF_FUNC(isAudioDeviceReady, 0, ),
    NIF_FUNC(setMasterVolume, 1, ),
    NIF_FUNC(getMasterVolume, 0, ),

    // Wave/Sound loading/unloading functions
    NIF_FUNC(loadWave, 1, ),
    NIF_FUNC(loadWaveFromMemory, 3, ),
    NIF_FUNC(isWaveReady, 1, ),
    NIF_FUNC(loadSound, 1, ),
    NIF_FUNC(loadSoundFromWave, 1, ),
    NIF_FUNC(loadSoundAlias, 1, ),
    NIF_FUNC(isSoundReady, 1, ),
    NIF_FUNC(updateSound, 3, ),
    NIF_FUNC(unloadWave, 1, ),
    NIF_FUNC(unloadSound, 1, ),
    NIF_FUNC(unloadSoundAlias, 1, ),
    NIF_FUNC(exportWave, 2, ),
    NIF_FUNC(exportWaveAsCode, 2, ),

    // Wave/Sound management functions
    NIF_FUNC(playSound, 1, ),
    NIF_FUNC(stopSound, 1, ),
    NIF_FUNC(pauseSound, 1, ),
    NIF_FUNC(resumeSound, 1, ),
    NIF_FUNC(isSoundPlaying, 1, ),
    NIF_FUNC(setSoundVolume, 2, ),
    NIF_FUNC(setSoundPitch, 2, ),
    NIF_FUNC(setSoundPan, 2, ),
    NIF_FUNC(waveCopy, 1, ),
    NIF_FUNC(waveCrop, 3, ),
    NIF_FUNC(waveFormat, 4, ),
    NIF_FUNC(loadWaveSamples, 1, ),
    NIF_FUNC(unloadWaveSamples, 1, ),

    // Music management functions
    NIF_FUNC(loadMusicStream, 1, ),
    NIF_FUNC(loadMusicStreamFromMemory, 3, ),
    NIF_FUNC(isMusicReady, 1, ),
    NIF_FUNC(unloadMusicStream, 1, ),
    NIF_FUNC(playMusicStream, 1, ),
    NIF_FUNC(isMusicStreamPlaying, 1, ),
    NIF_FUNC(updateMusicStream, 1, ),
    NIF_FUNC(stopMusicStream, 1, ),
    NIF_FUNC(pauseMusicStream, 1, ),
    NIF_FUNC(resumeMusicStream, 1, ),
    NIF_FUNC(seekMusicStream, 2, ),
    NIF_FUNC(setMusicVolume, 2, ),
    NIF_FUNC(setMusicPitch, 2, ),
    NIF_FUNC(setMusicPan, 2, ),
    NIF_FUNC(getMusicTimeLength, 1, ),
    NIF_FUNC(getMusicTimePlayed, 1, ),

    // AudioStream management functions
    NIF_FUNC(loadAudioStream, 3, ),
    NIF_FUNC(isAudioStreamReady, 1, ),
    NIF_FUNC(unloadAudioStream, 1, ),
    NIF_FUNC(updateAudioStream, 3, ),
    NIF_FUNC(isAudioStreamProcessed, 1, ),
    NIF_FUNC(playAudioStream, 1, ),
    NIF_FUNC(pauseAudioStream, 1, ),
    NIF_FUNC(resumeAudioStream, 1, ),
    NIF_FUNC(isAudioStreamPlaying, 1, ),
    NIF_FUNC(stopAudioStream, 1, ),
    NIF_FUNC(setAudioStreamVolume, 2, ),
    NIF_FUNC(setAudioStreamPitch, 2, ),
    NIF_FUNC(setAudioStreamPan, 2, ),
    NIF_FUNC(setAudioStreamBufferSizeDefault, 1, ),
    NIF_FUNC(setAudioStreamCallback, 2, ),

    NIF_FUNC(attachAudioStreamProcessor, 2, ),
    NIF_FUNC(detachAudioStreamProcessor, 2, ),

    NIF_FUNC(attachAudioMixedProcessor, 1, ),
    NIF_FUNC(detachAudioMixedProcessor, 1, ),
};

ERL_NIF_INIT(erlaylib, nif_funcs, NULL, NULL, NULL, NULL);