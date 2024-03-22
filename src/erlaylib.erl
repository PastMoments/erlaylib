-module(erlaylib).
-export([
    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: rcore
    %% Window-related functions
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

    %% Cursor-related functions
    showCursor/0,
    hideCursor/0,
    isCursorHidden/0,
    enableCursor/0,
    disableCursor/0,
    isCursorOnScreen/0,

    %% Drawing-related functions
    clearBackground/1,
    beginDrawing/0,
    endDrawing/0,
    beginMode2D/1,
    endMode2D/0,
    beginMode3D/1,
    endMode3D/0,
    beginTextureMode/1,
    endTextureMode/0,
    beginShaderMode/1,
    endShaderMode/0,
    beginBlendMode/1,
    endBlendMode/0,
    beginScissorMode/4,
    endScissorMode/0,
    beginVrStereoMode/1,
    endVrStereoMode/0,

    %% VR stereo config functions for VR simulator
    loadVrStereoConfig/1,
    unloadVrStereoConfig/1,

    %% Shader management functions
    %% NOTE: Shader functionality is not available on OpenGL 1.1
    loadShader/2,
    loadShaderFromMemory/2,
    isShaderReady/1,
    getShaderLocation/2,
    getShaderLocationAttrib/2,
    setShaderValue/4,
    setShaderValueV/5,
    setShaderValueMatrix/3,
    setShaderValueTexture/3,
    unloadShader/1,

    %% Screen-space-related functions
    getMouseRay/2,
    getCameraMatrix/1,
    getCameraMatrix2D/1,
    getWorldToScreen/2,
    getScreenToWorld2D/2,
    getWorldToScreenEx/4,
    getWorldToScreen2D/2,

    %% Timing-related functions
    setTargetFPS/1,
    getFrameTime/0,
    getTime/0,
    getFPS/0,

    %% Custom frame control functions
    %% NOTE: Those functions are intended for advance users that want full control over the frame processing
    %% By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timing + PollInputEvents()
    %% To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
    swapScreenBuffer/0,
    pollInputEvents/0,
    waitTime/1,

    %% Random values generation functions
    setRandomSeed/1,
    getRandomValue/2,
    loadRandomSequence/3,
    unloadRandomSequence/1,

    %% Misc. functions
    takeScreenshot/1,
    setConfigFlags/1,
    openURL/1,

    %% NOTE: Following functions implemented in module [utils]
    %%------------------------------------------------------------------
    traceLog/3,
    setTraceLogLevel/1,
    memAlloc/1,
    memRealloc/2,
    memFree/1,

    %% Set custom callbacks
    %% WARNING: Callbacks setup is intended for advance users
    setTraceLogCallback/1,
    setLoadFileDataCallback/1,
    setSaveFileDataCallback/1,
    setLoadFileTextCallback/1,
    setSaveFileTextCallback/1,

    %% Files management functions
    loadFileData/2,
    unloadFileData/1,
    saveFileData/3,
    exportDataAsCode/3,
    loadFileText/1,
    unloadFileText/1,
    saveFileText/2,
    %%------------------------------------------------------------------

    %% File system functions
    fileExists/1,
    directoryExists/1,
    isFileExtension/2,
    getFileLength/1,
    getFileExtension/1,
    getFileName/1,
    getFileNameWithoutExt/1,
    getDirectoryPath/1,
    getPrevDirectoryPath/1,
    getWorkingDirectory/0,
    getApplicationDirectory/0,
    changeDirectory/1,
    isPathFile/1,
    loadDirectoryFiles/1,
    loadDirectoryFilesEx/3,
    unloadDirectoryFiles/1,
    isFileDropped/0,
    loadDroppedFiles/0,
    unloadDroppedFiles/1,
    getFileModTime/1,

    %% Compression/Encoding functionality
    compressData/3,
    decompressData/3,
    encodeDataBase64/3,
    decodeDataBase64/2,

    %% Automation events functionality
    loadAutomationEventList/1,
    unloadAutomationEventList/1,
    exportAutomationEventList/2,
    setAutomationEventList/1,
    setAutomationEventBaseFrame/1,
    startAutomationEventRecording/0,
    stopAutomationEventRecording/0,
    playAutomationEvent/1,

    %%------------------------------------------------------------------------------------
    %% Input Handling Functions (Module: core)
    %%------------------------------------------------------------------------------------

    %% Input-related functions: keyboard
    isKeyPressed/1,
    isKeyPressedRepeat/1,
    isKeyDown/1,
    isKeyReleased/1,
    isKeyUp/1,
    getKeyPressed/0,
    getCharPressed/0,
    setExitKey/1,

    %% Input-related functions: gamepads
    isGamepadAvailable/1,
    getGamepadName/1,
    isGamepadButtonPressed/2,
    isGamepadButtonDown/2,
    isGamepadButtonReleased/2,
    isGamepadButtonUp/2,
    getGamepadButtonPressed/0,
    getGamepadAxisCount/1,
    getGamepadAxisMovement/2,
    setGamepadMappings/1,

    %% Input-related functions: mouse
    isMouseButtonPressed/1,
    isMouseButtonDown/1,
    isMouseButtonReleased/1,
    isMouseButtonUp/1,
    getMouseX/0,
    getMouseY/0,
    getMousePosition/0,
    getMouseDelta/0,
    setMousePosition/2,
    setMouseOffset/2,
    setMouseScale/2,
    getMouseWheelMove/0,
    getMouseWheelMoveV/0,
    setMouseCursor/1,

    %% Input-related functions: touch
    getTouchX/0,
    getTouchY/0,
    getTouchPosition/1,
    getTouchPointId/1,
    getTouchPointCount/0,

    %%------------------------------------------------------------------------------------
    %% Gestures and Touch Handling Functions (Module: rgestures)
    %%------------------------------------------------------------------------------------
    setGesturesEnabled/1,
    isGestureDetected/1,
    getGestureDetected/0,
    getGestureHoldDuration/0,
    getGestureDragVector/0,
    getGestureDragAngle/0,
    getGesturePinchVector/0,
    getGesturePinchAngle/0,

    %%------------------------------------------------------------------------------------
    %% Camera System Functions (Module: rcamera)
    %%------------------------------------------------------------------------------------
    updateCamera/2,
    updateCameraPro/4,

    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: rshapes

    %% NOTE: It can be useful when using basic shapes and one single font,
    %% defining a font char white rectangle would allow drawing everything in a single draw call
    setShapesTexture/2,

    %% Basic shapes drawing functions
    drawPixel/3,
    drawPixelV/2,
    drawLine/5,
    drawLineV/3,
    drawLineEx/4,
    drawLineStrip/3,
    drawLineBezier/4,
    drawCircle/4,
    drawCircleSector/6,
    drawCircleSectorLines/6,
    drawCircleGradient/5,
    drawCircleV/3,
    drawCircleLines/4,
    drawCircleLinesV/3,
    drawEllipse/5,
    drawEllipseLines/5,
    drawRing/7,
    drawRingLines/7,
    drawRectangle/5,
    drawRectangleV/3,
    drawRectangleRec/2,
    drawRectanglePro/4,
    drawRectangleGradientV/6,
    drawRectangleGradientH/6,
    drawRectangleGradientEx/5,
    drawRectangleLines/5,
    drawRectangleLinesEx/3,
    drawRectangleRounded/4,
    drawRectangleRoundedLines/5,
    drawTriangle/4,
    drawTriangleLines/4,
    drawTriangleFan/3,
    drawTriangleStrip/3,
    drawPoly/5,
    drawPolyLines/5,
    drawPolyLinesEx/6,

    %% Splines drawing functions
    drawSplineLinear/4,
    drawSplineBasis/4,
    drawSplineCatmullRom/4,
    drawSplineBezierQuadratic/4,
    drawSplineBezierCubic/4,
    drawSplineSegmentLinear/4,
    drawSplineSegmentBasis/6,
    drawSplineSegmentCatmullRom/6,
    drawSplineSegmentBezierQuadratic/5,
    drawSplineSegmentBezierCubic/6,

    %% Spline segment point evaluation functions, for a given t [0.0f .. 1.0f]
    getSplinePointLinear/3,
    getSplinePointBasis/5,
    getSplinePointCatmullRom/5,
    getSplinePointBezierQuad/4,
    getSplinePointBezierCubic/5,

    %% Basic shapes collision detection functions
    checkCollisionRecs/2,
    checkCollisionCircles/4,
    checkCollisionCircleRec/3,
    checkCollisionPointRec/2,
    checkCollisionPointCircle/3,
    checkCollisionPointTriangle/4,
    checkCollisionPointPoly/3,
    checkCollisionLines/5,
    checkCollisionPointLine/4,
    getCollisionRec/2,

    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: rtextures

    %% Image loading functions
    %% NOTE: These functions do not require GPU access
    loadImage/1,
    loadImageRaw/5,
    loadImageSvg/3,
    loadImageAnim/2,
    loadImageFromMemory/3,
    loadImageFromTexture/1,
    loadImageFromScreen/0,
    isImageReady/1,
    unloadImage/1,
    exportImage/2,
    exportImageToMemory/3,
    exportImageAsCode/2,

    %% Image generation functions
    genImageColor/3,
    genImageGradientLinear/5,
    genImageGradientRadial/5,
    genImageGradientSquare/5,
    genImageChecked/6,
    genImageWhiteNoise/3,
    genImagePerlinNoise/5,
    genImageCellular/3,
    genImageText/3,

    %% Image manipulation functions
    imageCopy/1,
    imageFromImage/2,
    imageText/3,
    imageTextEx/5,
    imageFormat/2,
    imageToPOT/2,
    imageCrop/2,
    imageAlphaCrop/2,
    imageAlphaClear/3,
    imageAlphaMask/2,
    imageAlphaPremultiply/1,
    imageBlurGaussian/2,
    imageResize/3,
    imageResizeNN/3,
    imageResizeCanvas/6,
    imageMipmaps/1,
    imageDither/5,
    imageFlipVertical/1,
    imageFlipHorizontal/1,
    imageRotate/2,
    imageRotateCW/1,
    imageRotateCCW/1,
    imageColorTint/2,
    imageColorInvert/1,
    imageColorGrayscale/1,
    imageColorContrast/2,
    imageColorBrightness/2,
    imageColorReplace/3,
    loadImageColors/1,
    loadImagePalette/3,
    unloadImageColors/1,
    unloadImagePalette/1,
    getImageAlphaBorder/2,
    getImageColor/3,

    %% Image drawing functions
    %% NOTE: Image software-rendering functions (CPU)
    imageClearBackground/2,
    imageDrawPixel/4,
    imageDrawPixelV/3,
    imageDrawLine/6,
    imageDrawLineV/4,
    imageDrawCircle/5,
    imageDrawCircleV/4,
    imageDrawCircleLines/5,
    imageDrawCircleLinesV/4,
    imageDrawRectangle/6,
    imageDrawRectangleV/4,
    imageDrawRectangleRec/3,
    imageDrawRectangleLines/4,
    imageDraw/5,
    imageDrawText/6,
    imageDrawTextEx/7,

    %% Texture loading functions
    %% NOTE: These functions require GPU access
    loadTexture/1,
    loadTextureFromImage/1,
    loadTextureCubemap/2,
    loadRenderTexture/2,
    isTextureReady/1,
    unloadTexture/1,
    isRenderTextureReady/1,
    unloadRenderTexture/1,
    updateTexture/2,
    updateTextureRec/3,

    %% Texture configuration functions
    genTextureMipmaps/1,
    setTextureFilter/2,
    setTextureWrap/2,

    %% Texture drawing functions
    drawTexture/4,
    drawTextureV/3,
    drawTextureEx/5,
    drawTextureRec/4,
    drawTexturePro/6,
    drawTextureNPatch/6,

    %% Color/pixel related functions
    fade/2,
    colorToInt/1,
    colorNormalize/1,
    colorFromNormalized/1,
    colorToHSV/1,
    colorFromHSV/3,
    colorTint/2,
    colorBrightness/2,
    colorContrast/2,
    colorAlpha/2,
    colorAlphaBlend/3,
    getColor/1,
    getPixelColor/2,
    setPixelColor/3,
    getPixelDataSize/3,

    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: rtext

    %% Font loading/unloading functions
    getFontDefault/0,
    loadFont/1,
    loadFontEx/4,
    loadFontFromImage/3,
    loadFontFromMemory/6,
    isFontReady/1,
    loadFontData/6,
    genImageFontAtlas/6,
    unloadFontData/2,
    unloadFont/1,
    exportFontAsCode/2,

    %% Text drawing functions
    drawFPS/2,
    drawText/5,
    drawTextEx/6,
    drawTextPro/8,
    drawTextCodepoint/5,
    drawTextCodepoints/7,

    %% Text font info functions
    setTextLineSpacing/1,
    measureText/2,
    measureTextEx/4,
    getGlyphIndex/2,
    getGlyphInfo/2,
    getGlyphAtlasRec/2,

    %% Text codepoints management functions (unicode characters)
    loadUTF8/2,
    unloadUTF8/1,
    loadCodepoints/2,
    unloadCodepoints/1,
    getCodepointCount/1,
    getCodepoint/2,
    getCodepointNext/2,
    getCodepointPrevious/2,
    codepointToUTF8/2,

    %% Text strings management functions (no UTF-8 strings, only byte chars)
    %% NOTE: Some strings allocate memory internally for returned strings, just be careful!
    textCopy/2,
    textIsEqual/2,
    textLength/1,
    textFormat/2,
    textSubtext/3,
    textReplace/3,
    textInsert/3,
    textJoin/3,
    textSplit/3,
    textAppend/3,
    textFindIndex/2,
    textToUpper/1,
    textToLower/1,
    textToPascal/1,
    textToInteger/1,

    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: rmodels

    %% Basic geometric 3D shapes drawing functions
    drawLine3D/3,
    drawPoint3D/2,
    drawCircle3D/5,
    drawTriangle3D/4,
    drawTriangleStrip3D/3,
    drawCube/5,
    drawCubeV/3,
    drawCubeWires/5,
    drawCubeWiresV/3,
    drawSphere/3,
    drawSphereEx/5,
    drawSphereWires/5,
    drawCylinder/6,
    drawCylinderEx/6,
    drawCylinderWires/6,
    drawCylinderWiresEx/6,
    drawCapsule/6,
    drawCapsuleWires/6,
    drawPlane/3,
    drawRay/2,
    drawGrid/2,

    %% Model management functions
    loadModel/1,
    loadModelFromMesh/1,
    isModelReady/1,
    unloadModel/1,
    getModelBoundingBox/1,

    %% Model drawing functions
    drawModel/4,
    drawModelEx/6,
    drawModelWires/4,
    drawModelWiresEx/6,
    drawBoundingBox/2,
    drawBillboard/5,
    drawBillboardRec/6,
    drawBillboardPro/9,

    %% Mesh management functions
    uploadMesh/2,
    updateMeshBuffer/5,
    unloadMesh/1,
    drawMesh/3,
    drawMeshInstanced/4,
    exportMesh/2,
    getMeshBoundingBox/1,
    genMeshTangents/1,

    %% Mesh generation functions
    genMeshPoly/2,
    genMeshPlane/4,
    genMeshCube/3,
    genMeshSphere/3,
    genMeshHemiSphere/3,
    genMeshCylinder/3,
    genMeshCone/3,
    genMeshTorus/4,
    genMeshKnot/4,
    genMeshHeightmap/2,
    genMeshCubicmap/2,

    %% Material loading/unloading functions
    loadMaterials/2,
    loadMaterialDefault/0,
    isMaterialReady/1,
    unloadMaterial/1,
    setMaterialTexture/3,
    setModelMeshMaterial/3,

    %% Model animations loading/unloading functions
    loadModelAnimations/2,
    updateModelAnimation/3,
    unloadModelAnimation/1,
    unloadModelAnimations/2,
    isModelAnimationValid/2,

    %% Collision detection functions
    checkCollisionSpheres/4,
    checkCollisionBoxes/2,
    checkCollisionBoxSphere/3,
    getRayCollisionSphere/3,
    getRayCollisionBox/2,
    getRayCollisionMesh/3,
    getRayCollisionTriangle/4,
    getRayCollisionQuad/5,

    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: raudio

    %% Audio device management functions
    initAudioDevice/0,
    closeAudioDevice/0,
    isAudioDeviceReady/0,
    setMasterVolume/1,
    getMasterVolume/0,

    %% Wave/Sound loading/unloading functions
    loadWave/1,
    loadWaveFromMemory/3,
    isWaveReady/1,
    loadSound/1,
    loadSoundFromWave/1,
    loadSoundAlias/1,
    isSoundReady/1,
    updateSound/3,
    unloadWave/1,
    unloadSound/1,
    unloadSoundAlias/1,
    exportWave/2,
    exportWaveAsCode/2,

    %% Wave/Sound management functions
    playSound/1,
    stopSound/1,
    pauseSound/1,
    resumeSound/1,
    isSoundPlaying/1,
    setSoundVolume/2,
    setSoundPitch/2,
    setSoundPan/2,
    waveCopy/1,
    waveCrop/3,
    waveFormat/4,
    loadWaveSamples/1,
    unloadWaveSamples/1,

    %% Music management functions
    loadMusicStream/1,
    loadMusicStreamFromMemory/3,
    isMusicReady/1,
    unloadMusicStream/1,
    playMusicStream/1,
    isMusicStreamPlaying/1,
    updateMusicStream/1,
    stopMusicStream/1,
    pauseMusicStream/1,
    resumeMusicStream/1,
    seekMusicStream/2,
    setMusicVolume/2,
    setMusicPitch/2,
    setMusicPan/2,
    getMusicTimeLength/1,
    getMusicTimePlayed/1,

    %% AudioStream management functions
    loadAudioStream/3,
    isAudioStreamReady/1,
    unloadAudioStream/1,
    updateAudioStream/3,
    isAudioStreamProcessed/1,
    playAudioStream/1,
    pauseAudioStream/1,
    resumeAudioStream/1,
    isAudioStreamPlaying/1,
    stopAudioStream/1,
    setAudioStreamVolume/2,
    setAudioStreamPitch/2,
    setAudioStreamPan/2,
    setAudioStreamBufferSizeDefault/1,
    setAudioStreamCallback/2,

    attachAudioStreamProcessor/2,
    detachAudioStreamProcessor/2,

    attachAudioMixedProcessor/1,
    detachAudioMixedProcessor/1
]).
-nifs([
    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: rcore

    %% Window-related functions
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

    %% Cursor-related functions
    showCursor/0,
    hideCursor/0,
    isCursorHidden/0,
    enableCursor/0,
    disableCursor/0,
    isCursorOnScreen/0,

    %% Drawing-related functions
    clearBackground/1,
    beginDrawing/0,
    endDrawing/0,
    beginMode2D/1,
    endMode2D/0,
    beginMode3D/1,
    endMode3D/0,
    beginTextureMode/1,
    endTextureMode/0,
    beginShaderMode/1,
    endShaderMode/0,
    beginBlendMode/1,
    endBlendMode/0,
    beginScissorMode/4,
    endScissorMode/0,
    beginVrStereoMode/1,
    endVrStereoMode/0,

    %% VR stereo config functions for VR simulator
    loadVrStereoConfig/1,
    unloadVrStereoConfig/1,

    %% Shader management functions
    %% NOTE: Shader functionality is not available on OpenGL 1.1
    loadShader/2,
    loadShaderFromMemory/2,
    isShaderReady/1,
    getShaderLocation/2,
    getShaderLocationAttrib/2,
    setShaderValue/4,
    setShaderValueV/5,
    setShaderValueMatrix/3,
    setShaderValueTexture/3,
    unloadShader/1,

    %% Screen-space-related functions
    getMouseRay/2,
    getCameraMatrix/1,
    getCameraMatrix2D/1,
    getWorldToScreen/2,
    getScreenToWorld2D/2,
    getWorldToScreenEx/4,
    getWorldToScreen2D/2,

    %% Timing-related functions
    setTargetFPS/1,
    getFrameTime/0,
    getTime/0,
    getFPS/0,

    %% Custom frame control functions
    %% NOTE: Those functions are intended for advance users that want full control over the frame processing
    %% By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timing + PollInputEvents()
    %% To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
    swapScreenBuffer/0,
    pollInputEvents/0,
    waitTime/1,

    %% Random values generation functions
    setRandomSeed/1,
    getRandomValue/2,
    loadRandomSequence/3,
    unloadRandomSequence/1,

    %% Misc. functions
    takeScreenshot/1,
    setConfigFlags/1,
    openURL/1,

    %% NOTE: Following functions implemented in module [utils]
    %%------------------------------------------------------------------
    traceLog/3,
    setTraceLogLevel/1,
    memAlloc/1,
    memRealloc/2,
    memFree/1,

    %% Set custom callbacks
    %% WARNING: Callbacks setup is intended for advance users
    setTraceLogCallback/1,
    setLoadFileDataCallback/1,
    setSaveFileDataCallback/1,
    setLoadFileTextCallback/1,
    setSaveFileTextCallback/1,

    %% Files management functions
    loadFileData/2,
    unloadFileData/1,
    saveFileData/3,
    exportDataAsCode/3,
    loadFileText/1,
    unloadFileText/1,
    saveFileText/2,
    %%------------------------------------------------------------------

    %% File system functions
    fileExists/1,
    directoryExists/1,
    isFileExtension/2,
    getFileLength/1,
    getFileExtension/1,
    getFileName/1,
    getFileNameWithoutExt/1,
    getDirectoryPath/1,
    getPrevDirectoryPath/1,
    getWorkingDirectory/0,
    getApplicationDirectory/0,
    changeDirectory/1,
    isPathFile/1,
    loadDirectoryFiles/1,
    loadDirectoryFilesEx/3,
    unloadDirectoryFiles/1,
    isFileDropped/0,
    loadDroppedFiles/0,
    unloadDroppedFiles/1,
    getFileModTime/1,

    %% Compression/Encoding functionality
    compressData/3,
    decompressData/3,
    encodeDataBase64/3,
    decodeDataBase64/2,

    %% Automation events functionality
    loadAutomationEventList/1,
    unloadAutomationEventList/1,
    exportAutomationEventList/2,
    setAutomationEventList/1,
    setAutomationEventBaseFrame/1,
    startAutomationEventRecording/0,
    stopAutomationEventRecording/0,
    playAutomationEvent/1,

    %%------------------------------------------------------------------------------------
    %% Input Handling Functions (Module: core)
    %%------------------------------------------------------------------------------------

    %% Input-related functions: keyboard
    isKeyPressed/1,
    isKeyPressedRepeat/1,
    isKeyDown/1,
    isKeyReleased/1,
    isKeyUp/1,
    getKeyPressed/0,
    getCharPressed/0,
    setExitKey/1,

    %% Input-related functions: gamepads
    isGamepadAvailable/1,
    getGamepadName/1,
    isGamepadButtonPressed/2,
    isGamepadButtonDown/2,
    isGamepadButtonReleased/2,
    isGamepadButtonUp/2,
    getGamepadButtonPressed/0,
    getGamepadAxisCount/1,
    getGamepadAxisMovement/2,
    setGamepadMappings/1,

    %% Input-related functions: mouse
    isMouseButtonPressed/1,
    isMouseButtonDown/1,
    isMouseButtonReleased/1,
    isMouseButtonUp/1,
    getMouseX/0,
    getMouseY/0,
    getMousePosition/0,
    getMouseDelta/0,
    setMousePosition/2,
    setMouseOffset/2,
    setMouseScale/2,
    getMouseWheelMove/0,
    getMouseWheelMoveV/0,
    setMouseCursor/1,

    %% Input-related functions: touch
    getTouchX/0,
    getTouchY/0,
    getTouchPosition/1,
    getTouchPointId/1,
    getTouchPointCount/0,

    %%------------------------------------------------------------------------------------
    %% Gestures and Touch Handling Functions (Module: rgestures)
    %%------------------------------------------------------------------------------------
    setGesturesEnabled/1,
    isGestureDetected/1,
    getGestureDetected/0,
    getGestureHoldDuration/0,
    getGestureDragVector/0,
    getGestureDragAngle/0,
    getGesturePinchVector/0,
    getGesturePinchAngle/0,

    %%------------------------------------------------------------------------------------
    %% Camera System Functions (Module: rcamera)
    %%------------------------------------------------------------------------------------
    updateCamera/2,
    updateCameraPro/4,

    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: rshapes

    %% NOTE: It can be useful when using basic shapes and one single font,
    %% defining a font char white rectangle would allow drawing everything in a single draw call
    setShapesTexture/2,

    %% Basic shapes drawing functions
    drawPixel/3,
    drawPixelV/2,
    drawLine/5,
    drawLineV/3,
    drawLineEx/4,
    drawLineStrip/3,
    drawLineBezier/4,
    drawCircle/4,
    drawCircleSector/6,
    drawCircleSectorLines/6,
    drawCircleGradient/5,
    drawCircleV/3,
    drawCircleLines/4,
    drawCircleLinesV/3,
    drawEllipse/5,
    drawEllipseLines/5,
    drawRing/7,
    drawRingLines/7,
    drawRectangle/5,
    drawRectangleV/3,
    drawRectangleRec/2,
    drawRectanglePro/4,
    drawRectangleGradientV/6,
    drawRectangleGradientH/6,
    drawRectangleGradientEx/5,
    drawRectangleLines/5,
    drawRectangleLinesEx/3,
    drawRectangleRounded/4,
    drawRectangleRoundedLines/5,
    drawTriangle/4,
    drawTriangleLines/4,
    drawTriangleFan/3,
    drawTriangleStrip/3,
    drawPoly/5,
    drawPolyLines/5,
    drawPolyLinesEx/6,

    %% Splines drawing functions
    drawSplineLinear/4,
    drawSplineBasis/4,
    drawSplineCatmullRom/4,
    drawSplineBezierQuadratic/4,
    drawSplineBezierCubic/4,
    drawSplineSegmentLinear/4,
    drawSplineSegmentBasis/6,
    drawSplineSegmentCatmullRom/6,
    drawSplineSegmentBezierQuadratic/5,
    drawSplineSegmentBezierCubic/6,

    %% Spline segment point evaluation functions, for a given t [0.0f .. 1.0f]
    getSplinePointLinear/3,
    getSplinePointBasis/5,
    getSplinePointCatmullRom/5,
    getSplinePointBezierQuad/4,
    getSplinePointBezierCubic/5,

    %% Basic shapes collision detection functions
    checkCollisionRecs/2,
    checkCollisionCircles/4,
    checkCollisionCircleRec/3,
    checkCollisionPointRec/2,
    checkCollisionPointCircle/3,
    checkCollisionPointTriangle/4,
    checkCollisionPointPoly/3,
    checkCollisionLines/5,
    checkCollisionPointLine/4,
    getCollisionRec/2,

    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: rtextures

    %% Image loading functions
    %% NOTE: These functions do not require GPU access
    loadImage/1,
    loadImageRaw/5,
    loadImageSvg/3,
    loadImageAnim/2,
    loadImageFromMemory/3,
    loadImageFromTexture/1,
    loadImageFromScreen/0,
    isImageReady/1,
    unloadImage/1,
    exportImage/2,
    exportImageToMemory/3,
    exportImageAsCode/2,

    %% Image generation functions
    genImageColor/3,
    genImageGradientLinear/5,
    genImageGradientRadial/5,
    genImageGradientSquare/5,
    genImageChecked/6,
    genImageWhiteNoise/3,
    genImagePerlinNoise/5,
    genImageCellular/3,
    genImageText/3,

    %% Image manipulation functions
    imageCopy/1,
    imageFromImage/2,
    imageText/3,
    imageTextEx/5,
    imageFormat/2,
    imageToPOT/2,
    imageCrop/2,
    imageAlphaCrop/2,
    imageAlphaClear/3,
    imageAlphaMask/2,
    imageAlphaPremultiply/1,
    imageBlurGaussian/2,
    imageResize/3,
    imageResizeNN/3,
    imageResizeCanvas/6,
    imageMipmaps/1,
    imageDither/5,
    imageFlipVertical/1,
    imageFlipHorizontal/1,
    imageRotate/2,
    imageRotateCW/1,
    imageRotateCCW/1,
    imageColorTint/2,
    imageColorInvert/1,
    imageColorGrayscale/1,
    imageColorContrast/2,
    imageColorBrightness/2,
    imageColorReplace/3,
    loadImageColors/1,
    loadImagePalette/3,
    unloadImageColors/1,
    unloadImagePalette/1,
    getImageAlphaBorder/2,
    getImageColor/3,

    %% Image drawing functions
    %% NOTE: Image software-rendering functions (CPU)
    imageClearBackground/2,
    imageDrawPixel/4,
    imageDrawPixelV/3,
    imageDrawLine/6,
    imageDrawLineV/4,
    imageDrawCircle/5,
    imageDrawCircleV/4,
    imageDrawCircleLines/5,
    imageDrawCircleLinesV/4,
    imageDrawRectangle/6,
    imageDrawRectangleV/4,
    imageDrawRectangleRec/3,
    imageDrawRectangleLines/4,
    imageDraw/5,
    imageDrawText/6,
    imageDrawTextEx/7,

    %% Texture loading functions
    %% NOTE: These functions require GPU access
    loadTexture/1,
    loadTextureFromImage/1,
    loadTextureCubemap/2,
    loadRenderTexture/2,
    isTextureReady/1,
    unloadTexture/1,
    isRenderTextureReady/1,
    unloadRenderTexture/1,
    updateTexture/2,
    updateTextureRec/3,

    %% Texture configuration functions
    genTextureMipmaps/1,
    setTextureFilter/2,
    setTextureWrap/2,

    %% Texture drawing functions
    drawTexture/4,
    drawTextureV/3,
    drawTextureEx/5,
    drawTextureRec/4,
    drawTexturePro/6,
    drawTextureNPatch/6,

    %% Color/pixel related functions
    fade/2,
    colorToInt/1,
    colorNormalize/1,
    colorFromNormalized/1,
    colorToHSV/1,
    colorFromHSV/3,
    colorTint/2,
    colorBrightness/2,
    colorContrast/2,
    colorAlpha/2,
    colorAlphaBlend/3,
    getColor/1,
    getPixelColor/2,
    setPixelColor/3,
    getPixelDataSize/3,

    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: rtext

    %% Font loading/unloading functions
    getFontDefault/0,
    loadFont/1,
    loadFontEx/4,
    loadFontFromImage/3,
    loadFontFromMemory/6,
    isFontReady/1,
    loadFontData/6,
    genImageFontAtlas/6,
    unloadFontData/2,
    unloadFont/1,
    exportFontAsCode/2,

    %% Text drawing functions
    drawFPS/2,
    drawText/5,
    drawTextEx/6,
    drawTextPro/8,
    drawTextCodepoint/5,
    drawTextCodepoints/7,

    %% Text font info functions
    setTextLineSpacing/1,
    measureText/2,
    measureTextEx/4,
    getGlyphIndex/2,
    getGlyphInfo/2,
    getGlyphAtlasRec/2,

    %% Text codepoints management functions (unicode characters)
    loadUTF8/2,
    unloadUTF8/1,
    loadCodepoints/2,
    unloadCodepoints/1,
    getCodepointCount/1,
    getCodepoint/2,
    getCodepointNext/2,
    getCodepointPrevious/2,
    codepointToUTF8/2,

    %% Text strings management functions (no UTF-8 strings, only byte chars)
    %% NOTE: Some strings allocate memory internally for returned strings, just be careful!
    textCopy/2,
    textIsEqual/2,
    textLength/1,
    textFormat/2,
    textSubtext/3,
    textReplace/3,
    textInsert/3,
    textJoin/3,
    textSplit/3,
    textAppend/3,
    textFindIndex/2,
    textToUpper/1,
    textToLower/1,
    textToPascal/1,
    textToInteger/1,

    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: rmodels

    %% Basic geometric 3D shapes drawing functions
    drawLine3D/3,
    drawPoint3D/2,
    drawCircle3D/5,
    drawTriangle3D/4,
    drawTriangleStrip3D/3,
    drawCube/5,
    drawCubeV/3,
    drawCubeWires/5,
    drawCubeWiresV/3,
    drawSphere/3,
    drawSphereEx/5,
    drawSphereWires/5,
    drawCylinder/6,
    drawCylinderEx/6,
    drawCylinderWires/6,
    drawCylinderWiresEx/6,
    drawCapsule/6,
    drawCapsuleWires/6,
    drawPlane/3,
    drawRay/2,
    drawGrid/2,

    %% Model management functions
    loadModel/1,
    loadModelFromMesh/1,
    isModelReady/1,
    unloadModel/1,
    getModelBoundingBox/1,

    %% Model drawing functions
    drawModel/4,
    drawModelEx/6,
    drawModelWires/4,
    drawModelWiresEx/6,
    drawBoundingBox/2,
    drawBillboard/5,
    drawBillboardRec/6,
    drawBillboardPro/9,

    %% Mesh management functions
    uploadMesh/2,
    updateMeshBuffer/5,
    unloadMesh/1,
    drawMesh/3,
    drawMeshInstanced/4,
    exportMesh/2,
    getMeshBoundingBox/1,
    genMeshTangents/1,

    %% Mesh generation functions
    genMeshPoly/2,
    genMeshPlane/4,
    genMeshCube/3,
    genMeshSphere/3,
    genMeshHemiSphere/3,
    genMeshCylinder/3,
    genMeshCone/3,
    genMeshTorus/4,
    genMeshKnot/4,
    genMeshHeightmap/2,
    genMeshCubicmap/2,

    %% Material loading/unloading functions
    loadMaterials/2,
    loadMaterialDefault/0,
    isMaterialReady/1,
    unloadMaterial/1,
    setMaterialTexture/3,
    setModelMeshMaterial/3,

    %% Model animations loading/unloading functions
    loadModelAnimations/2,
    updateModelAnimation/3,
    unloadModelAnimation/1,
    unloadModelAnimations/2,
    isModelAnimationValid/2,

    %% Collision detection functions
    checkCollisionSpheres/4,
    checkCollisionBoxes/2,
    checkCollisionBoxSphere/3,
    getRayCollisionSphere/3,
    getRayCollisionBox/2,
    getRayCollisionMesh/3,
    getRayCollisionTriangle/4,
    getRayCollisionQuad/5,

    %%% -----------------------------------------------------------------------------------
    %%%
    %%% module: raudio

    %% Audio device management functions
    initAudioDevice/0,
    closeAudioDevice/0,
    isAudioDeviceReady/0,
    setMasterVolume/1,
    getMasterVolume/0,

    %% Wave/Sound loading/unloading functions
    loadWave/1,
    loadWaveFromMemory/3,
    isWaveReady/1,
    loadSound/1,
    loadSoundFromWave/1,
    loadSoundAlias/1,
    isSoundReady/1,
    updateSound/3,
    unloadWave/1,
    unloadSound/1,
    unloadSoundAlias/1,
    exportWave/2,
    exportWaveAsCode/2,

    %% Wave/Sound management functions
    playSound/1,
    stopSound/1,
    pauseSound/1,
    resumeSound/1,
    isSoundPlaying/1,
    setSoundVolume/2,
    setSoundPitch/2,
    setSoundPan/2,
    waveCopy/1,
    waveCrop/3,
    waveFormat/4,
    loadWaveSamples/1,
    unloadWaveSamples/1,

    %% Music management functions
    loadMusicStream/1,
    loadMusicStreamFromMemory/3,
    isMusicReady/1,
    unloadMusicStream/1,
    playMusicStream/1,
    isMusicStreamPlaying/1,
    updateMusicStream/1,
    stopMusicStream/1,
    pauseMusicStream/1,
    resumeMusicStream/1,
    seekMusicStream/2,
    setMusicVolume/2,
    setMusicPitch/2,
    setMusicPan/2,
    getMusicTimeLength/1,
    getMusicTimePlayed/1,

    %% AudioStream management functions
    loadAudioStream/3,
    isAudioStreamReady/1,
    unloadAudioStream/1,
    updateAudioStream/3,
    isAudioStreamProcessed/1,
    playAudioStream/1,
    pauseAudioStream/1,
    resumeAudioStream/1,
    isAudioStreamPlaying/1,
    stopAudioStream/1,
    setAudioStreamVolume/2,
    setAudioStreamPitch/2,
    setAudioStreamPan/2,
    setAudioStreamBufferSizeDefault/1,
    setAudioStreamCallback/2,

    attachAudioStreamProcessor/2,
    detachAudioStreamProcessor/2,

    attachAudioMixedProcessor/1,
    detachAudioMixedProcessor/1
]).
-on_load(init/0).

init() ->
    erlang:load_nif("priv/erlaylib", 0).

-define(not_loaded, erlang:nif_error("NIF library not loaded")).



    
initWindow(_Width,_Height,_Title) ->
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
isWindowState(_Flag) ->
    ?not_loaded.                      
setWindowState(_Flags) ->
    ?not_loaded.                    
clearWindowState(_Flags) ->
    ?not_loaded.                  
toggleFullscreen() ->
    ?not_loaded.                                
toggleBorderlessWindowed() ->
    ?not_loaded.                        
maximizeWindow() ->
    ?not_loaded.                                  
minimizeWindow() ->
    ?not_loaded.                                  
restoreWindow() ->
    ?not_loaded.                                   
setWindowIcon(_Image) ->
    ?not_loaded.                            
setWindowIcons(_Images,_Count) ->
    ?not_loaded.              
setWindowTitle(_Title) ->
    ?not_loaded.                     
setWindowPosition(_X,_Y) ->
    ?not_loaded.                       
setWindowMonitor(_Monitor) ->
    ?not_loaded.                         
setWindowMinSize(_Width,_Height) ->
    ?not_loaded.               
setWindowMaxSize(_Width,_Height) ->
    ?not_loaded.               
setWindowSize(_Width,_Height) ->
    ?not_loaded.                  
setWindowOpacity(_Opacity) ->
    ?not_loaded.                       
setWindowFocused() ->
    ?not_loaded.                                
getWindowHandle() ->
    ?not_loaded.                                
getScreenWidth() ->
    ?not_loaded.                                   
getScreenHeight() ->
    ?not_loaded.                                  
getRenderWidth() ->
    ?not_loaded.                                   
getRenderHeight() ->
    ?not_loaded.                                  
getMonitorCount() ->
    ?not_loaded.                                  
getCurrentMonitor() ->
    ?not_loaded.                                
getMonitorPosition(_Monitor) ->
    ?not_loaded.                    
getMonitorWidth(_Monitor) ->
    ?not_loaded.                           
getMonitorHeight(_Monitor) ->
    ?not_loaded.                          
getMonitorPhysicalWidth(_Monitor) ->
    ?not_loaded.                   
getMonitorPhysicalHeight(_Monitor) ->
    ?not_loaded.                  
getMonitorRefreshRate(_Monitor) ->
    ?not_loaded.                     
getWindowPosition() ->
    ?not_loaded.                            
getWindowScaleDPI() ->
    ?not_loaded.                            
getMonitorName(_Monitor) ->
    ?not_loaded.                    
setClipboardText(_Text) ->
    ?not_loaded.                    
getClipboardText() ->
    ?not_loaded.                         
enableEventWaiting() ->
    ?not_loaded.                              
disableEventWaiting() ->
    ?not_loaded.                             

    
showCursor() ->
    ?not_loaded.                                      
hideCursor() ->
    ?not_loaded.                                      
isCursorHidden() ->
    ?not_loaded.                                  
enableCursor() ->
    ?not_loaded.                                    
disableCursor() ->
    ?not_loaded.                                   
isCursorOnScreen() ->
    ?not_loaded.                                

    
clearBackground(_Color) ->
    ?not_loaded.                          
beginDrawing() ->
    ?not_loaded.                                    
endDrawing() ->
    ?not_loaded.                                      
beginMode2D(_Camera) ->
    ?not_loaded.                          
endMode2D() ->
    ?not_loaded.                                       
beginMode3D(_Camera) ->
    ?not_loaded.                          
endMode3D() ->
    ?not_loaded.                                       
beginTextureMode(_Target) ->
    ?not_loaded.              
endTextureMode() ->
    ?not_loaded.                                  
beginShaderMode(_Shader) ->
    ?not_loaded.                        
endShaderMode() ->
    ?not_loaded.                                   
beginBlendMode(_Mode) ->
    ?not_loaded.                              
endBlendMode() ->
    ?not_loaded.                                    
beginScissorMode(_X,_Y,_Width,_Height) ->
    ?not_loaded. 
endScissorMode() ->
    ?not_loaded.                                  
beginVrStereoMode(_Config) ->
    ?not_loaded.              
endVrStereoMode() ->
    ?not_loaded.                                 

    
loadVrStereoConfig(_Device) ->
    ?not_loaded.     
unloadVrStereoConfig(_Config) ->
    ?not_loaded.           

    
    
loadShader(_VsFileName,_FsFileName) ->
    ?not_loaded.   
loadShaderFromMemory(_VsCode,_FsCode) ->
    ?not_loaded. 
isShaderReady(_Shader) ->
    ?not_loaded.                                   
getShaderLocation(_Shader,_UniformName) ->
    ?not_loaded.       
getShaderLocationAttrib(_Shader,_AttribName) ->
    ?not_loaded.  
setShaderValue(_Shader,_LocIndex,_Value,_UniformType) ->
    ?not_loaded.               
setShaderValueV(_Shader,_LocIndex,_Value,_UniformType,_Count) ->
    ?not_loaded.   
setShaderValueMatrix(_Shader,_LocIndex,_Mat) ->
    ?not_loaded.         
setShaderValueTexture(_Shader,_LocIndex,_Texture) ->
    ?not_loaded. 
unloadShader(_Shader) ->
    ?not_loaded.                                    

    
getMouseRay(_MousePosition,_Camera) ->
    ?not_loaded.      
getCameraMatrix(_Camera) ->
    ?not_loaded.                      
getCameraMatrix2D(_Camera) ->
    ?not_loaded.                  
getWorldToScreen(_Position,_Camera) ->
    ?not_loaded.  
getScreenToWorld2D(_Position,_Camera) ->
    ?not_loaded. 
getWorldToScreenEx(_Position,_Camera,_Width,_Height) ->
    ?not_loaded. 
getWorldToScreen2D(_Position,_Camera) ->
    ?not_loaded. 

    
setTargetFPS(_Fps) ->
    ?not_loaded.                                 
getFrameTime() ->
    ?not_loaded.                                   
getTime() ->
    ?not_loaded.                                       
getFPS() ->
    ?not_loaded.                                           

    
    
    
    
swapScreenBuffer() ->
    ?not_loaded.                                
pollInputEvents() ->
    ?not_loaded.                                 
waitTime(_Seconds) ->
    ?not_loaded.                              

    
setRandomSeed(_Seed) ->
    ?not_loaded.                      
getRandomValue(_Min,_Max) ->
    ?not_loaded.                       
loadRandomSequence(_Count,_Min,_Max) ->
    ?not_loaded. 
unloadRandomSequence(_Sequence) ->
    ?not_loaded.                   

    
takeScreenshot(_FileName) ->
    ?not_loaded.                  
setConfigFlags(_Flags) ->
    ?not_loaded.                    
openURL(_Url) ->
    ?not_loaded.                              

    
    
traceLog(_LogLevel,_Text, _Args) ->
    ?not_loaded.         
setTraceLogLevel(_LogLevel) ->
    ?not_loaded.                        
memAlloc(_Size) ->
    ?not_loaded.                          
memRealloc(_Ptr,_Size) ->
    ?not_loaded.             
memFree(_Ptr) ->
    ?not_loaded.                                    

    
    
setTraceLogCallback(_Callback) ->
    ?not_loaded.         
setLoadFileDataCallback(_Callback) ->
    ?not_loaded. 
setSaveFileDataCallback(_Callback) ->
    ?not_loaded. 
setLoadFileTextCallback(_Callback) ->
    ?not_loaded. 
setSaveFileTextCallback(_Callback) ->
    ?not_loaded. 

    
loadFileData(_FileName,_DataSize) ->
    ?not_loaded. 
unloadFileData(_Data) ->
    ?not_loaded.                   
saveFileData(_FileName,_Data,_DataSize) ->
    ?not_loaded. 
exportDataAsCode(_Data,_DataSize,_FileName) ->
    ?not_loaded. 
loadFileText(_FileName) ->
    ?not_loaded.                   
unloadFileText(_Text) ->
    ?not_loaded.                            
saveFileText(_FileName,_Text) ->
    ?not_loaded.        
    

    
fileExists(_FileName) ->
    ?not_loaded.                      
directoryExists(_DirPath) ->
    ?not_loaded.                  
isFileExtension(_FileName,_Ext) ->
    ?not_loaded. 
getFileLength(_FileName) ->
    ?not_loaded.                    
getFileExtension(_FileName) ->
    ?not_loaded.         
getFileName(_FilePath) ->
    ?not_loaded.              
getFileNameWithoutExt(_FilePath) ->
    ?not_loaded.    
getDirectoryPath(_FilePath) ->
    ?not_loaded.         
getPrevDirectoryPath(_DirPath) ->
    ?not_loaded.      
getWorkingDirectory() ->
    ?not_loaded.                      
getApplicationDirectory() ->
    ?not_loaded.                  
changeDirectory(_Dir) ->
    ?not_loaded.                      
isPathFile(_Path) ->
    ?not_loaded.                          
loadDirectoryFiles(_DirPath) ->
    ?not_loaded.       
loadDirectoryFilesEx(_BasePath,_Filter,_ScanSubdirs) ->
    ?not_loaded. 
unloadDirectoryFiles(_Files) ->
    ?not_loaded.              
isFileDropped() ->
    ?not_loaded.                                   
loadDroppedFiles() ->
    ?not_loaded.                        
unloadDroppedFiles(_Files) ->
    ?not_loaded.                
getFileModTime(_FileName) ->
    ?not_loaded.                  

    
compressData(_Data,_DataSize,_CompDataSize) ->
    ?not_loaded.        
decompressData(_CompData,_CompDataSize,_DataSize) ->
    ?not_loaded.  
encodeDataBase64(_Data,_DataSize,_OutputSize) ->
    ?not_loaded.               
decodeDataBase64(_Data,_OutputSize) ->
    ?not_loaded.                    

    
loadAutomationEventList(_FileName) ->
    ?not_loaded.                
unloadAutomationEventList(_List) ->
    ?not_loaded.                        
exportAutomationEventList(_List,_FileName) ->
    ?not_loaded.   
setAutomationEventList(_List) ->
    ?not_loaded.                           
setAutomationEventBaseFrame(_Frame) ->
    ?not_loaded.                                      
startAutomationEventRecording() ->
    ?not_loaded.                                         
stopAutomationEventRecording() ->
    ?not_loaded.                                          
playAutomationEvent(_Event) ->
    ?not_loaded.                                  

    
    
    

    
isKeyPressed(_Key) ->
    ?not_loaded.                             
isKeyPressedRepeat(_Key) ->
    ?not_loaded.                       
isKeyDown(_Key) ->
    ?not_loaded.                                
isKeyReleased(_Key) ->
    ?not_loaded.                            
isKeyUp(_Key) ->
    ?not_loaded.                                  
getKeyPressed() ->
    ?not_loaded.                                
getCharPressed() ->
    ?not_loaded.                               
setExitKey(_Key) ->
    ?not_loaded.                               

    
isGamepadAvailable(_Gamepad) ->
    ?not_loaded.                   
getGamepadName(_Gamepad) ->
    ?not_loaded.                
isGamepadButtonPressed(_Gamepad,_Button) ->
    ?not_loaded.   
isGamepadButtonDown(_Gamepad,_Button) ->
    ?not_loaded.      
isGamepadButtonReleased(_Gamepad,_Button) ->
    ?not_loaded.  
isGamepadButtonUp(_Gamepad,_Button) ->
    ?not_loaded.        
getGamepadButtonPressed() ->
    ?not_loaded.                      
getGamepadAxisCount(_Gamepad) ->
    ?not_loaded.                   
getGamepadAxisMovement(_Gamepad,_Axis) ->
    ?not_loaded.    
setGamepadMappings(_Mappings) ->
    ?not_loaded.           

    
isMouseButtonPressed(_Button) ->
    ?not_loaded.                  
isMouseButtonDown(_Button) ->
    ?not_loaded.                     
isMouseButtonReleased(_Button) ->
    ?not_loaded.                 
isMouseButtonUp(_Button) ->
    ?not_loaded.                       
getMouseX() ->
    ?not_loaded.                                    
getMouseY() ->
    ?not_loaded.                                    
getMousePosition() ->
    ?not_loaded.                         
getMouseDelta() ->
    ?not_loaded.                            
setMousePosition(_X,_Y) ->
    ?not_loaded.                    
setMouseOffset(_OffsetX,_OffsetY) ->
    ?not_loaded.          
setMouseScale(_ScaleX,_ScaleY) ->
    ?not_loaded.         
getMouseWheelMove() ->
    ?not_loaded.                          
getMouseWheelMoveV() ->
    ?not_loaded.                       
setMouseCursor(_Cursor) ->
    ?not_loaded.                        

    
getTouchX() ->
    ?not_loaded.                                    
getTouchY() ->
    ?not_loaded.                                    
getTouchPosition(_Index) ->
    ?not_loaded.                    
getTouchPointId(_Index) ->
    ?not_loaded.                         
getTouchPointCount() ->
    ?not_loaded.                           

    
    
    
setGesturesEnabled(_Flags) ->
    ?not_loaded.      
isGestureDetected(_Gesture) ->
    ?not_loaded.     
getGestureDetected() ->
    ?not_loaded.                     
getGestureHoldDuration() ->
    ?not_loaded.               
getGestureDragVector() ->
    ?not_loaded.               
getGestureDragAngle() ->
    ?not_loaded.                  
getGesturePinchVector() ->
    ?not_loaded.              
getGesturePinchAngle() ->
    ?not_loaded.                 

    
    
    
updateCamera(_Camera,_Mode) ->
    ?not_loaded.      
updateCameraPro(_Camera,_Movement,_Rotation,_Zoom) ->
    ?not_loaded. 



    
    
setShapesTexture(_Texture,_Source) ->
    ?not_loaded.       

    
drawPixel(_PosX,_PosY,_Color) ->
    ?not_loaded.                                                   
drawPixelV(_Position,_Color) ->
    ?not_loaded.                                                    
drawLine(_StartPosX,_StartPosY,_EndPosX,_EndPosY,_Color) ->
    ?not_loaded.                
drawLineV(_StartPos,_EndPos,_Color) ->
    ?not_loaded.                                     
drawLineEx(_StartPos,_EndPos,_Thick,_Color) ->
    ?not_loaded.                       
drawLineStrip(_Points,_PointCount,_Color) ->
    ?not_loaded.                                  
drawLineBezier(_StartPos,_EndPos,_Thick,_Color) ->
    ?not_loaded.                   
drawCircle(_CenterX,_CenterY,_Radius,_Color) ->
    ?not_loaded.                              
drawCircleSector(_Center,_Radius,_StartAngle,_EndAngle,_Segments,_Color) ->
    ?not_loaded.      
drawCircleSectorLines(_Center,_Radius,_StartAngle,_EndAngle,_Segments,_Color) ->
    ?not_loaded. 
drawCircleGradient(_CenterX,_CenterY,_Radius,_Color1,_Color2) ->
    ?not_loaded.       
drawCircleV(_Center,_Radius,_Color) ->
    ?not_loaded.                                       
drawCircleLines(_CenterX,_CenterY,_Radius,_Color) ->
    ?not_loaded.                         
drawCircleLinesV(_Center,_Radius,_Color) ->
    ?not_loaded.                                  
drawEllipse(_CenterX,_CenterY,_RadiusH,_RadiusV,_Color) ->
    ?not_loaded.             
drawEllipseLines(_CenterX,_CenterY,_RadiusH,_RadiusV,_Color) ->
    ?not_loaded.        
drawRing(_Center,_InnerRadius,_OuterRadius,_StartAngle,_EndAngle,_Segments,_Color) ->
    ?not_loaded. 
drawRingLines(_Center,_InnerRadius,_OuterRadius,_StartAngle,_EndAngle,_Segments,_Color) ->
    ?not_loaded.    
drawRectangle(_PosX,_PosY,_Width,_Height,_Color) ->
    ?not_loaded.                        
drawRectangleV(_Position,_Size,_Color) ->
    ?not_loaded.                                  
drawRectangleRec(_Rec,_Color) ->
    ?not_loaded.                                                 
drawRectanglePro(_Rec,_Origin,_Rotation,_Color) ->
    ?not_loaded.                 
drawRectangleGradientV(_PosX,_PosY,_Width,_Height,_Color1,_Color2) ->
    ?not_loaded.
drawRectangleGradientH(_PosX,_PosY,_Width,_Height,_Color1,_Color2) ->
    ?not_loaded.
drawRectangleGradientEx(_Rec,_Col1,_Col2,_Col3,_Col4) ->
    ?not_loaded.       
drawRectangleLines(_PosX,_PosY,_Width,_Height,_Color) ->
    ?not_loaded.                   
drawRectangleLinesEx(_Rec,_LineThick,_Color) ->
    ?not_loaded.                            
drawRectangleRounded(_Rec,_Roundness,_Segments,_Color) ->
    ?not_loaded.              
drawRectangleRoundedLines(_Rec,_Roundness,_Segments,_LineThick,_Color) ->
    ?not_loaded. 
drawTriangle(_V1,_V2,_V3,_Color) ->
    ?not_loaded.                                
drawTriangleLines(_V1,_V2,_V3,_Color) ->
    ?not_loaded.                           
drawTriangleFan(_Points,_PointCount,_Color) ->
    ?not_loaded.                                
drawTriangleStrip(_Points,_PointCount,_Color) ->
    ?not_loaded.                              
drawPoly(_Center,_Sides,_Radius,_Rotation,_Color) ->
    ?not_loaded.               
drawPolyLines(_Center,_Sides,_Radius,_Rotation,_Color) ->
    ?not_loaded.          
drawPolyLinesEx(_Center,_Sides,_Radius,_Rotation,_LineThick,_Color) ->
    ?not_loaded. 

    
drawSplineLinear(_Points,_PointCount,_Thick,_Color) ->
    ?not_loaded.                  
drawSplineBasis(_Points,_PointCount,_Thick,_Color) ->
    ?not_loaded.                   
drawSplineCatmullRom(_Points,_PointCount,_Thick,_Color) ->
    ?not_loaded.              
drawSplineBezierQuadratic(_Points,_PointCount,_Thick,_Color) ->
    ?not_loaded.         
drawSplineBezierCubic(_Points,_PointCount,_Thick,_Color) ->
    ?not_loaded.             
drawSplineSegmentLinear(_P1,_P2,_Thick,_Color) ->
    ?not_loaded.                    
drawSplineSegmentBasis(_P1,_P2,_P3,_P4,_Thick,_Color) ->
    ?not_loaded. 
drawSplineSegmentCatmullRom(_P1,_P2,_P3,_P4,_Thick,_Color) ->
    ?not_loaded. 
drawSplineSegmentBezierQuadratic(_P1,_C2,_P3,_Thick,_Color) ->
    ?not_loaded. 
drawSplineSegmentBezierCubic(_P1,_C2,_C3,_P4,_Thick,_Color) ->
    ?not_loaded. 

    
getSplinePointLinear(_StartPos,_EndPos,_T) ->
    ?not_loaded.                           
getSplinePointBasis(_P1,_P2,_P3,_P4,_T) ->
    ?not_loaded.              
getSplinePointCatmullRom(_P1,_P2,_P3,_P4,_T) ->
    ?not_loaded.         
getSplinePointBezierQuad(_P1,_C2,_P3,_T) ->
    ?not_loaded.                     
getSplinePointBezierCubic(_P1,_C2,_C3,_P4,_T) ->
    ?not_loaded.        

    
checkCollisionRecs(_Rec1,_Rec2) ->
    ?not_loaded.                                           
checkCollisionCircles(_Center1,_Radius1,_Center2,_Radius2) ->
    ?not_loaded.        
checkCollisionCircleRec(_Center,_Radius,_Rec) ->
    ?not_loaded.                         
checkCollisionPointRec(_Point,_Rec) ->
    ?not_loaded.                                         
checkCollisionPointCircle(_Point,_Center,_Radius) ->
    ?not_loaded.                       
checkCollisionPointTriangle(_Point,_P1,_P2,_P3) ->
    ?not_loaded.               
checkCollisionPointPoly(_Point,_Points,_PointCount) ->
    ?not_loaded.                      
checkCollisionLines(_StartPos1,_EndPos1,_StartPos2,_EndPos2,_CollisionPoint) ->
    ?not_loaded. 
checkCollisionPointLine(_Point,_P1,_P2,_Threshold) ->
    ?not_loaded.                
getCollisionRec(_Rec1,_Rec2) ->
    ?not_loaded.                                         



    
    
loadImage(_FileName) ->
    ?not_loaded.                                                             
loadImageRaw(_FileName,_Width,_Height,_Format,_HeaderSize) ->
    ?not_loaded.       
loadImageSvg(_FileNameOrString,_Width,_Height) ->
    ?not_loaded.                           
loadImageAnim(_FileName,_Frames) ->
    ?not_loaded.                                            
loadImageFromMemory(_FileType,_FileData,_DataSize) ->
    ?not_loaded.      
loadImageFromTexture(_Texture) ->
    ?not_loaded.                                                     
loadImageFromScreen() ->
    ?not_loaded.                                                                   
isImageReady(_Image) ->
    ?not_loaded.                                                                    
unloadImage(_Image) ->
    ?not_loaded.                                                                     
exportImage(_Image,_FileName) ->
    ?not_loaded.                                               
exportImageToMemory(_Image,_FileType,_FileSize) ->
    ?not_loaded.              
exportImageAsCode(_Image,_FileName) ->
    ?not_loaded.                                         

    
genImageColor(_Width,_Height,_Color) ->
    ?not_loaded.                                           
genImageGradientLinear(_Width,_Height,_Direction,_Start,_End) ->
    ?not_loaded.        
genImageGradientRadial(_Width,_Height,_Density,_Inner,_Outer) ->
    ?not_loaded.      
genImageGradientSquare(_Width,_Height,_Density,_Inner,_Outer) ->
    ?not_loaded.      
genImageChecked(_Width,_Height,_ChecksX,_ChecksY,_Col1,_Col2) ->
    ?not_loaded.    
genImageWhiteNoise(_Width,_Height,_Factor) ->
    ?not_loaded.                                     
genImagePerlinNoise(_Width,_Height,_OffsetX,_OffsetY,_Scale) ->
    ?not_loaded.           
genImageCellular(_Width,_Height,_TileSize) ->
    ?not_loaded.                                       
genImageText(_Width,_Height,_Text) ->
    ?not_loaded.                                       

    
imageCopy(_Image) ->
    ?not_loaded.                                                                      
imageFromImage(_Image,_Rec) ->
    ?not_loaded.                                                  
imageText(_Text,_FontSize,_Color) ->
    ?not_loaded.                                      
imageTextEx(_Font,_Text,_FontSize,_Spacing,_Tint) ->
    ?not_loaded.         
imageFormat(_Image,_NewFormat) ->
    ?not_loaded.                                                     
imageToPOT(_Image,_Fill) ->
    ?not_loaded.                                                         
imageCrop(_Image,_Crop) ->
    ?not_loaded.                                                      
imageAlphaCrop(_Image,_Threshold) ->
    ?not_loaded.                                                
imageAlphaClear(_Image,_Color,_Threshold) ->
    ?not_loaded.                                  
imageAlphaMask(_Image,_AlphaMask) ->
    ?not_loaded.                                                
imageAlphaPremultiply(_Image) ->
    ?not_loaded.                                                          
imageBlurGaussian(_Image,_BlurSize) ->
    ?not_loaded.                                                
imageResize(_Image,_NewWidth,_NewHeight) ->
    ?not_loaded.                                       
imageResizeNN(_Image,_NewWidth,_NewHeight) ->
    ?not_loaded.                                      
imageResizeCanvas(_Image,_NewWidth,_NewHeight,_OffsetX,_OffsetY,_Fill) ->
    ?not_loaded.  
imageMipmaps(_Image) ->
    ?not_loaded.                                                                   
imageDither(_Image,_RBpp,_GBpp,_BBpp,_ABpp) ->
    ?not_loaded.                            
imageFlipVertical(_Image) ->
    ?not_loaded.                                                              
imageFlipHorizontal(_Image) ->
    ?not_loaded.                                                            
imageRotate(_Image,_Degrees) ->
    ?not_loaded.                                                       
imageRotateCW(_Image) ->
    ?not_loaded.                                                                  
imageRotateCCW(_Image) ->
    ?not_loaded.                                                                 
imageColorTint(_Image,_Color) ->
    ?not_loaded.                                                    
imageColorInvert(_Image) ->
    ?not_loaded.                                                               
imageColorGrayscale(_Image) ->
    ?not_loaded.                                                            
imageColorContrast(_Image,_Contrast) ->
    ?not_loaded.                                             
imageColorBrightness(_Image,_Brightness) ->
    ?not_loaded.                                           
imageColorReplace(_Image,_Color,_Replace) ->
    ?not_loaded.                                  
loadImageColors(_Image) ->
    ?not_loaded.                                                               
loadImagePalette(_Image,_MaxPaletteSize,_ColorCount) ->
    ?not_loaded.                         
unloadImageColors(_Colors) ->
    ?not_loaded.                                                             
unloadImagePalette(_Colors) ->
    ?not_loaded.                                                            
getImageAlphaBorder(_Image,_Threshold) ->
    ?not_loaded.                                       
getImageColor(_Image,_X,_Y) ->
    ?not_loaded.                                                    

    
    
imageClearBackground(_Dst,_Color) ->
    ?not_loaded.                                                
imageDrawPixel(_Dst,_PosX,_PosY,_Color) ->
    ?not_loaded.                                  
imageDrawPixelV(_Dst,_Position,_Color) ->
    ?not_loaded.                                   
imageDrawLine(_Dst,_StartPosX,_StartPosY,_EndPosX,_EndPosY,_Color) ->
    ?not_loaded. 
imageDrawLineV(_Dst,_Start,_End,_Color) ->
    ?not_loaded.                          
imageDrawCircle(_Dst,_CenterX,_CenterY,_Radius,_Color) ->
    ?not_loaded.               
imageDrawCircleV(_Dst,_Center,_Radius,_Color) ->
    ?not_loaded.                        
imageDrawCircleLines(_Dst,_CenterX,_CenterY,_Radius,_Color) ->
    ?not_loaded.          
imageDrawCircleLinesV(_Dst,_Center,_Radius,_Color) ->
    ?not_loaded.                   
imageDrawRectangle(_Dst,_PosX,_PosY,_Width,_Height,_Color) ->
    ?not_loaded.       
imageDrawRectangleV(_Dst,_Position,_Size,_Color) ->
    ?not_loaded.                 
imageDrawRectangleRec(_Dst,_Rec,_Color) ->
    ?not_loaded.                                
imageDrawRectangleLines(_Dst,_Rec,_Thick,_Color) ->
    ?not_loaded.                   
imageDraw(_Dst,_Src,_SrcRec,_DstRec,_Tint) ->
    ?not_loaded.             
imageDrawText(_Dst,_Text,_PosX,_PosY,_FontSize,_Color) ->
    ?not_loaded.   
imageDrawTextEx(_Dst,_Font,_Text,_Position,_FontSize,_Spacing,_Tint) ->
    ?not_loaded. 

    
    
loadTexture(_FileName) ->
    ?not_loaded.                                                       
loadTextureFromImage(_Image) ->
    ?not_loaded.                                                       
loadTextureCubemap(_Image,_Layout) ->
    ?not_loaded.                                        
loadRenderTexture(_Width,_Height) ->
    ?not_loaded.                                          
isTextureReady(_Texture) ->
    ?not_loaded.                                                            
unloadTexture(_Texture) ->
    ?not_loaded.                                                             
isRenderTextureReady(_Target) ->
    ?not_loaded.                                                 
unloadRenderTexture(_Target) ->
    ?not_loaded.                                                  
updateTexture(_Texture,_Pixels) ->
    ?not_loaded.                                         
updateTextureRec(_Texture,_Rec,_Pixels) ->
    ?not_loaded.                       

    
genTextureMipmaps(_Texture) ->
    ?not_loaded.                                                        
setTextureFilter(_Texture,_Filter) ->
    ?not_loaded.                                              
setTextureWrap(_Texture,_Wrap) ->
    ?not_loaded.                                                  

    
drawTexture(_Texture,_PosX,_PosY,_Tint) ->
    ?not_loaded.                               
drawTextureV(_Texture,_Position,_Tint) ->
    ?not_loaded.                                
drawTextureEx(_Texture,_Position,_Rotation,_Scale,_Tint) ->
    ?not_loaded.  
drawTextureRec(_Texture,_Source,_Position,_Tint) ->
    ?not_loaded.            
drawTexturePro(_Texture,_Source,_Dest,_Origin,_Rotation,_Tint) ->
    ?not_loaded. 
drawTextureNPatch(_Texture,_NPatchInfo,_Dest,_Origin,_Rotation,_Tint) ->
    ?not_loaded. 

    
fade(_Color,_Alpha) ->
    ?not_loaded.                                 
colorToInt(_Color) ->
    ?not_loaded.                                          
colorNormalize(_Color) ->
    ?not_loaded.                                  
colorFromNormalized(_Normalized) ->
    ?not_loaded.                        
colorToHSV(_Color) ->
    ?not_loaded.                                      
colorFromHSV(_Hue,_Saturation,_Value) ->
    ?not_loaded.         
colorTint(_Color,_Tint) ->
    ?not_loaded.                             
colorBrightness(_Color,_Factor) ->
    ?not_loaded.                     
colorContrast(_Color,_Contrast) ->
    ?not_loaded.                     
colorAlpha(_Color,_Alpha) ->
    ?not_loaded.                           
colorAlphaBlend(_Dst,_Src,_Tint) ->
    ?not_loaded.              
getColor(_HexValue) ->
    ?not_loaded.                                
getPixelColor(_SrcPtr,_Format) ->
    ?not_loaded.                        
setPixelColor(_DstPtr,_Color,_Format) ->
    ?not_loaded.            
getPixelDataSize(_Width,_Height,_Format) ->
    ?not_loaded.              



    
getFontDefault() ->
    ?not_loaded.                                                            
loadFont(_FileName) ->
    ?not_loaded.                                                  
loadFontEx(_FileName,_FontSize,_Codepoints,_CodepointCount) ->
    ?not_loaded.  
loadFontFromImage(_Image,_Key,_FirstChar) ->
    ?not_loaded.                        
loadFontFromMemory(_FileType,_FileData,_DataSize,_FontSize,_Codepoints,_CodepointCount) ->
    ?not_loaded. 
isFontReady(_Font) ->
    ?not_loaded.                                                          
loadFontData(_FileData,_DataSize,_FontSize,_Codepoints,_CodepointCount,_Type) ->
    ?not_loaded. 
genImageFontAtlas(_Glyphs,_GlyphRecs,_GlyphCount,_FontSize,_Padding,_PackMethod) ->
    ?not_loaded. 
unloadFontData(_Glyphs,_GlyphCount) ->
    ?not_loaded.                               
unloadFont(_Font) ->
    ?not_loaded.                                                           
exportFontAsCode(_Font,_FileName) ->
    ?not_loaded.                               

    
drawFPS(_PosX,_PosY) ->
    ?not_loaded.                                                     
drawText(_Text,_PosX,_PosY,_FontSize,_Color) ->
    ?not_loaded.       
drawTextEx(_Font,_Text,_Position,_FontSize,_Spacing,_Tint) ->
    ?not_loaded. 
drawTextPro(_Font,_Text,_Position,_Origin,_Rotation,_FontSize,_Spacing,_Tint) ->
    ?not_loaded. 
drawTextCodepoint(_Font,_Codepoint,_Position,_FontSize,_Tint) ->
    ?not_loaded. 
drawTextCodepoints(_Font,_Codepoints,_CodepointCount,_Position,_FontSize,_Spacing,_Tint) ->
    ?not_loaded. 

    
setTextLineSpacing(_Spacing) ->
    ?not_loaded.                                                 
measureText(_Text,_FontSize) ->
    ?not_loaded.                                      
measureTextEx(_Font,_Text,_FontSize,_Spacing) ->
    ?not_loaded.    
getGlyphIndex(_Font,_Codepoint) ->
    ?not_loaded.                                          
getGlyphInfo(_Font,_Codepoint) ->
    ?not_loaded.                                     
getGlyphAtlasRec(_Font,_Codepoint) ->
    ?not_loaded.                                 

    
loadUTF8(_Codepoints,_Length) ->
    ?not_loaded.                
unloadUTF8(_Text) ->
    ?not_loaded.                                      
loadCodepoints(_Text,_Count) ->
    ?not_loaded.                
unloadCodepoints(_Codepoints) ->
    ?not_loaded.                           
getCodepointCount(_Text) ->
    ?not_loaded.                          
getCodepoint(_Text,_CodepointSize) ->
    ?not_loaded.           
getCodepointNext(_Text,_CodepointSize) ->
    ?not_loaded.       
getCodepointPrevious(_Text,_CodepointSize) ->
    ?not_loaded.   
codepointToUTF8(_Codepoint,_Utf8Size) ->
    ?not_loaded.        

    
    
textCopy(_Dst,_Src) ->
    ?not_loaded.                                             
textIsEqual(_Text1,_Text2) ->
    ?not_loaded.                               
textLength(_Text) ->
    ?not_loaded.                                            
textFormat(_Text, _Args) ->
    ?not_loaded.                                        
textSubtext(_Text,_Position,_Length) ->
    ?not_loaded.                  
textReplace(_Text,_Replace,_By) ->
    ?not_loaded.                   
textInsert(_Text,_Insert,_Position) ->
    ?not_loaded.                 
textJoin(_TextList,_Count,_Delimiter) ->
    ?not_loaded.        
textSplit(_Text,_Delimiter,_Count) ->
    ?not_loaded.                 
textAppend(_Text,_Append,_Position) ->
    ?not_loaded.                       
textFindIndex(_Text,_Find) ->
    ?not_loaded.                                
textToUpper(_Text) ->
    ?not_loaded.                      
textToLower(_Text) ->
    ?not_loaded.                      
textToPascal(_Text) ->
    ?not_loaded.                     
textToInteger(_Text) ->
    ?not_loaded.                            



    
drawLine3D(_StartPos,_EndPos,_Color) ->
    ?not_loaded.                                    
drawPoint3D(_Position,_Color) ->
    ?not_loaded.                                                   
drawCircle3D(_Center,_Radius,_RotationAxis,_RotationAngle,_Color) ->
    ?not_loaded. 
drawTriangle3D(_V1,_V2,_V3,_Color) ->
    ?not_loaded.                              
drawTriangleStrip3D(_Points,_PointCount,_Color) ->
    ?not_loaded.                            
drawCube(_Position,_Width,_Height,_Length,_Color) ->
    ?not_loaded.             
drawCubeV(_Position,_Size,_Color) ->
    ?not_loaded.                                       
drawCubeWires(_Position,_Width,_Height,_Length,_Color) ->
    ?not_loaded.        
drawCubeWiresV(_Position,_Size,_Color) ->
    ?not_loaded.                                  
drawSphere(_CenterPos,_Radius,_Color) ->
    ?not_loaded.                                     
drawSphereEx(_CenterPos,_Radius,_Rings,_Slices,_Color) ->
    ?not_loaded.            
drawSphereWires(_CenterPos,_Radius,_Rings,_Slices,_Color) ->
    ?not_loaded.         
drawCylinder(_Position,_RadiusTop,_RadiusBottom,_Height,_Slices,_Color) ->
    ?not_loaded. 
drawCylinderEx(_StartPos,_EndPos,_StartRadius,_EndRadius,_Sides,_Color) ->
    ?not_loaded. 
drawCylinderWires(_Position,_RadiusTop,_RadiusBottom,_Height,_Slices,_Color) ->
    ?not_loaded. 
drawCylinderWiresEx(_StartPos,_EndPos,_StartRadius,_EndRadius,_Sides,_Color) ->
    ?not_loaded. 
drawCapsule(_StartPos,_EndPos,_Radius,_Slices,_Rings,_Color) ->
    ?not_loaded. 
drawCapsuleWires(_StartPos,_EndPos,_Radius,_Slices,_Rings,_Color) ->
    ?not_loaded. 
drawPlane(_CenterPos,_Size,_Color) ->
    ?not_loaded.                                      
drawRay(_Ray,_Color) ->
    ?not_loaded.                                                                
drawGrid(_Slices,_Spacing) ->
    ?not_loaded.                                                          

    
    
    

    
loadModel(_FileName) ->
    ?not_loaded.                                                
loadModelFromMesh(_Mesh) ->
    ?not_loaded.                                                   
isModelReady(_Model) ->
    ?not_loaded.                                                       
unloadModel(_Model) ->
    ?not_loaded.                                                        
getModelBoundingBox(_Model) ->
    ?not_loaded.                                         

    
drawModel(_Model,_Position,_Scale,_Tint) ->
    ?not_loaded.               
drawModelEx(_Model,_Position,_RotationAxis,_RotationAngle,_Scale,_Tint) ->
    ?not_loaded. 
drawModelWires(_Model,_Position,_Scale,_Tint) ->
    ?not_loaded.          
drawModelWiresEx(_Model,_Position,_RotationAxis,_RotationAngle,_Scale,_Tint) ->
    ?not_loaded. 
drawBoundingBox(_Box,_Color) ->
    ?not_loaded.                                   
drawBillboard(_Camera,_Texture,_Position,_Size,_Tint) ->
    ?not_loaded.   
drawBillboardRec(_Camera,_Texture,_Source,_Position,_Size,_Tint) ->
    ?not_loaded. 
drawBillboardPro(_Camera,_Texture,_Source,_Position,_Up,_Size,_Origin,_Rotation,_Tint) ->
    ?not_loaded. 

    
uploadMesh(_Mesh,_Dynamic) ->
    ?not_loaded.                                            
updateMeshBuffer(_Mesh,_Index,_Data,_DataSize,_Offset) ->
    ?not_loaded. 
unloadMesh(_Mesh) ->
    ?not_loaded.                                                           
drawMesh(_Mesh,_Material,_Transform) ->
    ?not_loaded.                        
drawMeshInstanced(_Mesh,_Material,_Transforms,_Instances) ->
    ?not_loaded. 
exportMesh(_Mesh,_FileName) ->
    ?not_loaded.                                     
getMeshBoundingBox(_Mesh) ->
    ?not_loaded.                                            
genMeshTangents(_Mesh) ->
    ?not_loaded.                                                     

    
genMeshPoly(_Sides,_Radius) ->
    ?not_loaded.                                            
genMeshPlane(_Width,_Length,_ResX,_ResZ) ->
    ?not_loaded.                     
genMeshCube(_Width,_Height,_Length) ->
    ?not_loaded.                            
genMeshSphere(_Radius,_Rings,_Slices) ->
    ?not_loaded.                              
genMeshHemiSphere(_Radius,_Rings,_Slices) ->
    ?not_loaded.                          
genMeshCylinder(_Radius,_Height,_Slices) ->
    ?not_loaded.                         
genMeshCone(_Radius,_Height,_Slices) ->
    ?not_loaded.                             
genMeshTorus(_Radius,_Size,_RadSeg,_Sides) ->
    ?not_loaded.                   
genMeshKnot(_Radius,_Size,_RadSeg,_Sides) ->
    ?not_loaded.                    
genMeshHeightmap(_Heightmap,_Size) ->
    ?not_loaded.                                 
genMeshCubicmap(_Cubicmap,_CubeSize) ->
    ?not_loaded.                               

    
loadMaterials(_FileName,_MaterialCount) ->
    ?not_loaded.                    
loadMaterialDefault() ->
    ?not_loaded.                                                   
isMaterialReady(_Material) ->
    ?not_loaded.                                              
unloadMaterial(_Material) ->
    ?not_loaded.                                               
setMaterialTexture(_Material,_MapType,_Texture) ->
    ?not_loaded.          
setModelMeshMaterial(_Model,_MeshId,_MaterialId) ->
    ?not_loaded.                  

    
loadModelAnimations(_FileName,_AnimCount) ->
    ?not_loaded.            
updateModelAnimation(_Model,_Anim,_Frame) ->
    ?not_loaded.               
unloadModelAnimation(_Anim) ->
    ?not_loaded.                                       
unloadModelAnimations(_Animations,_AnimCount) ->
    ?not_loaded.                
isModelAnimationValid(_Model,_Anim) ->
    ?not_loaded.                         

    
checkCollisionSpheres(_Center1,_Radius1,_Center2,_Radius2) ->
    ?not_loaded.   
checkCollisionBoxes(_Box1,_Box2) ->
    ?not_loaded.                                 
checkCollisionBoxSphere(_Box,_Center,_Radius) ->
    ?not_loaded.                  
getRayCollisionSphere(_Ray,_Center,_Radius) ->
    ?not_loaded.                    
getRayCollisionBox(_Ray,_Box) ->
    ?not_loaded.                                    
getRayCollisionMesh(_Ray,_Mesh,_Transform) ->
    ?not_loaded.                       
getRayCollisionTriangle(_Ray,_P1,_P2,_P3) ->
    ?not_loaded.            
getRayCollisionQuad(_Ray,_P1,_P2,_P3,_P4) ->
    ?not_loaded.    



    
initAudioDevice() ->
    ?not_loaded.                                     
closeAudioDevice() ->
    ?not_loaded.                                    
isAudioDeviceReady() ->
    ?not_loaded.                                  
setMasterVolume(_Volume) ->
    ?not_loaded.                             
getMasterVolume() ->
    ?not_loaded.                                    

    
loadWave(_FileName) ->
    ?not_loaded.                            
loadWaveFromMemory(_FileType,_FileData,_DataSize) ->
    ?not_loaded. 
isWaveReady(_Wave) ->
    ?not_loaded.                                    
loadSound(_FileName) ->
    ?not_loaded.                          
loadSoundFromWave(_Wave) ->
    ?not_loaded.                             
loadSoundAlias(_Source) ->
    ?not_loaded.                             
isSoundReady(_Sound) ->
    ?not_loaded.                                 
updateSound(_Sound,_Data,_SampleCount) ->
    ?not_loaded. 
unloadWave(_Wave) ->
    ?not_loaded.                                     
unloadSound(_Sound) ->
    ?not_loaded.                                  
unloadSoundAlias(_Alias) ->
    ?not_loaded.                             
exportWave(_Wave,_FileName) ->
    ?not_loaded.               
exportWaveAsCode(_Wave,_FileName) ->
    ?not_loaded.         

    
playSound(_Sound) ->
    ?not_loaded.                                    
stopSound(_Sound) ->
    ?not_loaded.                                    
pauseSound(_Sound) ->
    ?not_loaded.                                   
resumeSound(_Sound) ->
    ?not_loaded.                                  
isSoundPlaying(_Sound) ->
    ?not_loaded.                               
setSoundVolume(_Sound,_Volume) ->
    ?not_loaded.                 
setSoundPitch(_Sound,_Pitch) ->
    ?not_loaded.                   
setSoundPan(_Sound,_Pan) ->
    ?not_loaded.                       
waveCopy(_Wave) ->
    ?not_loaded.                                       
waveCrop(_Wave,_InitSample,_FinalSample) ->
    ?not_loaded.     
waveFormat(_Wave,_SampleRate,_SampleSize,_Channels) ->
    ?not_loaded. 
loadWaveSamples(_Wave) ->
    ?not_loaded.                              
unloadWaveSamples(_Samples) ->
    ?not_loaded.                         

    
loadMusicStream(_FileName) ->
    ?not_loaded.                    
loadMusicStreamFromMemory(_FileType,_Data,_DataSize) ->
    ?not_loaded. 
isMusicReady(_Music) ->
    ?not_loaded.                                 
unloadMusicStream(_Music) ->
    ?not_loaded.                            
playMusicStream(_Music) ->
    ?not_loaded.                              
isMusicStreamPlaying(_Music) ->
    ?not_loaded.                         
updateMusicStream(_Music) ->
    ?not_loaded.                            
stopMusicStream(_Music) ->
    ?not_loaded.                              
pauseMusicStream(_Music) ->
    ?not_loaded.                             
resumeMusicStream(_Music) ->
    ?not_loaded.                            
seekMusicStream(_Music,_Position) ->
    ?not_loaded.              
setMusicVolume(_Music,_Volume) ->
    ?not_loaded.                 
setMusicPitch(_Music,_Pitch) ->
    ?not_loaded.                   
setMusicPan(_Music,_Pan) ->
    ?not_loaded.                       
getMusicTimeLength(_Music) ->
    ?not_loaded.                          
getMusicTimePlayed(_Music) ->
    ?not_loaded.                          

    
loadAudioStream(_SampleRate,_SampleSize,_Channels) ->
    ?not_loaded. 
isAudioStreamReady(_Stream) ->
    ?not_loaded.                    
unloadAudioStream(_Stream) ->
    ?not_loaded.                     
updateAudioStream(_Stream,_Data,_FrameCount) ->
    ?not_loaded. 
isAudioStreamProcessed(_Stream) ->
    ?not_loaded.                
playAudioStream(_Stream) ->
    ?not_loaded.                       
pauseAudioStream(_Stream) ->
    ?not_loaded.                      
resumeAudioStream(_Stream) ->
    ?not_loaded.                     
isAudioStreamPlaying(_Stream) ->
    ?not_loaded.                  
stopAudioStream(_Stream) ->
    ?not_loaded.                       
setAudioStreamVolume(_Stream,_Volume) ->
    ?not_loaded.    
setAudioStreamPitch(_Stream,_Pitch) ->
    ?not_loaded.      
setAudioStreamPan(_Stream,_Pan) ->
    ?not_loaded.          
setAudioStreamBufferSizeDefault(_Size) ->
    ?not_loaded.                 
setAudioStreamCallback(_Stream,_Callback) ->
    ?not_loaded. 

attachAudioStreamProcessor(_Stream,_Processor) ->
    ?not_loaded. 
detachAudioStreamProcessor(_Stream,_Processor) ->
    ?not_loaded. 

attachAudioMixedProcessor(_Processor) ->
    ?not_loaded. 
detachAudioMixedProcessor(_Processor) ->
    ?not_loaded. 

