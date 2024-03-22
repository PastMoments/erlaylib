#include "erl_nif.h"

#define DECLARE_NIF_FUNC(name) ERL_NIF_TERM NIF##name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])

// rcore
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
DECLARE_NIF_FUNC(clearWindowState);
DECLARE_NIF_FUNC(toggleFullscreen);
DECLARE_NIF_FUNC(toggleBorderlessWindowed);
DECLARE_NIF_FUNC(maximizeWindow);
DECLARE_NIF_FUNC(minimizeWindow);
DECLARE_NIF_FUNC(restoreWindow);
DECLARE_NIF_FUNC(setWindowIcon);
DECLARE_NIF_FUNC(setWindowIcons);
DECLARE_NIF_FUNC(setWindowTitle);
DECLARE_NIF_FUNC(setWindowPosition);
DECLARE_NIF_FUNC(setWindowMonitor);
DECLARE_NIF_FUNC(setWindowMinSize);
DECLARE_NIF_FUNC(setWindowMaxSize);
DECLARE_NIF_FUNC(setWindowSize);
DECLARE_NIF_FUNC(setWindowOpacity);
DECLARE_NIF_FUNC(setWindowFocused);
DECLARE_NIF_FUNC(getWindowHandle);
DECLARE_NIF_FUNC(getScreenWidth);
DECLARE_NIF_FUNC(getScreenHeight);
DECLARE_NIF_FUNC(getRenderWidth);
DECLARE_NIF_FUNC(getRenderHeight);
DECLARE_NIF_FUNC(getMonitorCount);
DECLARE_NIF_FUNC(getCurrentMonitor);
DECLARE_NIF_FUNC(getMonitorPosition);
DECLARE_NIF_FUNC(getMonitorWidth);
DECLARE_NIF_FUNC(getMonitorHeight);
DECLARE_NIF_FUNC(getMonitorPhysicalWidth);
DECLARE_NIF_FUNC(getMonitorPhysicalHeight);
DECLARE_NIF_FUNC(getMonitorRefreshRate);
DECLARE_NIF_FUNC(getWindowPosition);
DECLARE_NIF_FUNC(getWindowScaleDPI);
DECLARE_NIF_FUNC(getMonitorName);
DECLARE_NIF_FUNC(setClipboardText);
DECLARE_NIF_FUNC(getClipboardText);
DECLARE_NIF_FUNC(enableEventWaiting);
DECLARE_NIF_FUNC(disableEventWaiting);

// Cursor-related functions
DECLARE_NIF_FUNC(showCursor);
DECLARE_NIF_FUNC(hideCursor);
DECLARE_NIF_FUNC(isCursorHidden);
DECLARE_NIF_FUNC(enableCursor);
DECLARE_NIF_FUNC(disableCursor);
DECLARE_NIF_FUNC(isCursorOnScreen);

// Drawing-related functions
DECLARE_NIF_FUNC(clearBackground);
DECLARE_NIF_FUNC(beginDrawing);
DECLARE_NIF_FUNC(endDrawing);
DECLARE_NIF_FUNC(beginMode2D);
DECLARE_NIF_FUNC(endMode2D);
DECLARE_NIF_FUNC(beginMode3D);
DECLARE_NIF_FUNC(endMode3D);
DECLARE_NIF_FUNC(beginTextureMode);
DECLARE_NIF_FUNC(endTextureMode);
DECLARE_NIF_FUNC(beginShaderMode);
DECLARE_NIF_FUNC(endShaderMode);
DECLARE_NIF_FUNC(beginBlendMode);
DECLARE_NIF_FUNC(endBlendMode);
DECLARE_NIF_FUNC(beginScissorMode);
DECLARE_NIF_FUNC(endScissorMode);
DECLARE_NIF_FUNC(beginVrStereoMode);
DECLARE_NIF_FUNC(endVrStereoMode);

// VR stereo config functions for VR simulator
DECLARE_NIF_FUNC(loadVrStereoConfig);
DECLARE_NIF_FUNC(unloadVrStereoConfig);

// Shader management functions
// NOTE: Shader functionality is not available on OpenGL 1.1
DECLARE_NIF_FUNC(loadShader);
DECLARE_NIF_FUNC(loadShaderFromMemory);
DECLARE_NIF_FUNC(isShaderReady);
DECLARE_NIF_FUNC(getShaderLocation);
DECLARE_NIF_FUNC(getShaderLocationAttrib);
DECLARE_NIF_FUNC(setShaderValue);
DECLARE_NIF_FUNC(setShaderValueV);
DECLARE_NIF_FUNC(setShaderValueMatrix);
DECLARE_NIF_FUNC(setShaderValueTexture);
DECLARE_NIF_FUNC(unloadShader);

// Screen-space-related functions
DECLARE_NIF_FUNC(getMouseRay);
DECLARE_NIF_FUNC(getCameraMatrix);
DECLARE_NIF_FUNC(getCameraMatrix2D);
DECLARE_NIF_FUNC(getWorldToScreen);
DECLARE_NIF_FUNC(getScreenToWorld2D);
DECLARE_NIF_FUNC(getWorldToScreenEx);
DECLARE_NIF_FUNC(getWorldToScreen2D);

// Timing-related functions
DECLARE_NIF_FUNC(setTargetFPS);
DECLARE_NIF_FUNC(getFrameTime);
DECLARE_NIF_FUNC(getTime);
DECLARE_NIF_FUNC(getFPS);

// Custom frame control functions
// NOTE: Those functions are intended for advance users that want full control over the frame processing
// By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timing + PollInputEvents()
// To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
DECLARE_NIF_FUNC(swapScreenBuffer);
DECLARE_NIF_FUNC(pollInputEvents);
DECLARE_NIF_FUNC(waitTime);

// Random values generation functions
DECLARE_NIF_FUNC(setRandomSeed);
DECLARE_NIF_FUNC(getRandomValue);
DECLARE_NIF_FUNC(loadRandomSequence);
DECLARE_NIF_FUNC(unloadRandomSequence);

// Misc. functions
DECLARE_NIF_FUNC(takeScreenshot);
DECLARE_NIF_FUNC(setConfigFlags);
DECLARE_NIF_FUNC(openURL);

// NOTE: Following functions implemented in module [utils]
//------------------------------------------------------------------
DECLARE_NIF_FUNC(traceLog);
DECLARE_NIF_FUNC(setTraceLogLevel);
DECLARE_NIF_FUNC(memAlloc);
DECLARE_NIF_FUNC(memRealloc);
DECLARE_NIF_FUNC(memFree);

// Set custom callbacks
// WARNING: Callbacks setup is intended for advance users
DECLARE_NIF_FUNC(setTraceLogCallback);
DECLARE_NIF_FUNC(setLoadFileDataCallback);
DECLARE_NIF_FUNC(setSaveFileDataCallback);
DECLARE_NIF_FUNC(setLoadFileTextCallback);
DECLARE_NIF_FUNC(setSaveFileTextCallback);

// Files management functions
DECLARE_NIF_FUNC(loadFileData);
DECLARE_NIF_FUNC(unloadFileData);
DECLARE_NIF_FUNC(saveFileData);
DECLARE_NIF_FUNC(exportDataAsCode);
DECLARE_NIF_FUNC(loadFileText);
DECLARE_NIF_FUNC(unloadFileText);
DECLARE_NIF_FUNC(saveFileText);
//------------------------------------------------------------------

// File system functions
DECLARE_NIF_FUNC(fileExists);
DECLARE_NIF_FUNC(directoryExists);
DECLARE_NIF_FUNC(isFileExtension);
DECLARE_NIF_FUNC(getFileLength);
DECLARE_NIF_FUNC(getFileExtension);
DECLARE_NIF_FUNC(getFileName);
DECLARE_NIF_FUNC(getFileNameWithoutExt);
DECLARE_NIF_FUNC(getDirectoryPath);
DECLARE_NIF_FUNC(getPrevDirectoryPath);
DECLARE_NIF_FUNC(getWorkingDirectory);
DECLARE_NIF_FUNC(getApplicationDirectory);
DECLARE_NIF_FUNC(changeDirectory);
DECLARE_NIF_FUNC(isPathFile);
DECLARE_NIF_FUNC(loadDirectoryFiles);
DECLARE_NIF_FUNC(loadDirectoryFilesEx);
DECLARE_NIF_FUNC(unloadDirectoryFiles);
DECLARE_NIF_FUNC(isFileDropped);
DECLARE_NIF_FUNC(loadDroppedFiles);
DECLARE_NIF_FUNC(unloadDroppedFiles);
DECLARE_NIF_FUNC(getFileModTime);

// Compression/Encoding functionality
DECLARE_NIF_FUNC(compressData);
DECLARE_NIF_FUNC(decompressData);
DECLARE_NIF_FUNC(encodeDataBase64);
DECLARE_NIF_FUNC(decodeDataBase64);

// Automation events functionality
DECLARE_NIF_FUNC(loadAutomationEventList);
DECLARE_NIF_FUNC(unloadAutomationEventList);
DECLARE_NIF_FUNC(exportAutomationEventList);
DECLARE_NIF_FUNC(setAutomationEventList);
DECLARE_NIF_FUNC(setAutomationEventBaseFrame);
DECLARE_NIF_FUNC(startAutomationEventRecording);
DECLARE_NIF_FUNC(stopAutomationEventRecording);
DECLARE_NIF_FUNC(playAutomationEvent);

//------------------------------------------------------------------------------------
// Input Handling Functions (Module: core)
//------------------------------------------------------------------------------------

// Input-related functions: keyboard
DECLARE_NIF_FUNC(isKeyPressed);
DECLARE_NIF_FUNC(isKeyPressedRepeat);
DECLARE_NIF_FUNC(isKeyDown);
DECLARE_NIF_FUNC(isKeyReleased);
DECLARE_NIF_FUNC(isKeyUp);
DECLARE_NIF_FUNC(getKeyPressed);
DECLARE_NIF_FUNC(getCharPressed);
DECLARE_NIF_FUNC(setExitKey);

// Input-related functions: gamepads
DECLARE_NIF_FUNC(isGamepadAvailable);
DECLARE_NIF_FUNC(getGamepadName);
DECLARE_NIF_FUNC(isGamepadButtonPressed);
DECLARE_NIF_FUNC(isGamepadButtonDown);
DECLARE_NIF_FUNC(isGamepadButtonReleased);
DECLARE_NIF_FUNC(isGamepadButtonUp);
DECLARE_NIF_FUNC(getGamepadButtonPressed);
DECLARE_NIF_FUNC(getGamepadAxisCount);
DECLARE_NIF_FUNC(getGamepadAxisMovement);
DECLARE_NIF_FUNC(setGamepadMappings);

// Input-related functions: mouse
DECLARE_NIF_FUNC(isMouseButtonPressed);
DECLARE_NIF_FUNC(isMouseButtonDown);
DECLARE_NIF_FUNC(isMouseButtonReleased);
DECLARE_NIF_FUNC(isMouseButtonUp);
DECLARE_NIF_FUNC(getMouseX);
DECLARE_NIF_FUNC(getMouseY);
DECLARE_NIF_FUNC(getMousePosition);
DECLARE_NIF_FUNC(getMouseDelta);
DECLARE_NIF_FUNC(setMousePosition);
DECLARE_NIF_FUNC(setMouseOffset);
DECLARE_NIF_FUNC(setMouseScale);
DECLARE_NIF_FUNC(getMouseWheelMove);
DECLARE_NIF_FUNC(getMouseWheelMoveV);
DECLARE_NIF_FUNC(setMouseCursor);

// Input-related functions: touch
DECLARE_NIF_FUNC(getTouchX);
DECLARE_NIF_FUNC(getTouchY);
DECLARE_NIF_FUNC(getTouchPosition);
DECLARE_NIF_FUNC(getTouchPointId);
DECLARE_NIF_FUNC(getTouchPointCount);

//------------------------------------------------------------------------------------
// Gestures and Touch Handling Functions (Module: rgestures)
//------------------------------------------------------------------------------------
DECLARE_NIF_FUNC(setGesturesEnabled);
DECLARE_NIF_FUNC(isGestureDetected);
DECLARE_NIF_FUNC(getGestureDetected);
DECLARE_NIF_FUNC(getGestureHoldDuration);
DECLARE_NIF_FUNC(getGestureDragVector);
DECLARE_NIF_FUNC(getGestureDragAngle);
DECLARE_NIF_FUNC(getGesturePinchVector);
DECLARE_NIF_FUNC(getGesturePinchAngle);

//------------------------------------------------------------------------------------
// Camera System Functions (Module: rcamera)
//------------------------------------------------------------------------------------
DECLARE_NIF_FUNC(updateCamera);
DECLARE_NIF_FUNC(updateCameraPro);

/// -----------------------------------------------------------------------------------
///
/// module: rshapes

// NOTE: It can be useful when using basic shapes and one single font,
// defining a font char white rectangle would allow drawing everything in a single draw call
DECLARE_NIF_FUNC(setShapesTexture);

// Basic shapes drawing functions
DECLARE_NIF_FUNC(drawPixel);
DECLARE_NIF_FUNC(drawPixelV);
DECLARE_NIF_FUNC(drawLine);
DECLARE_NIF_FUNC(drawLineV);
DECLARE_NIF_FUNC(drawLineEx);
DECLARE_NIF_FUNC(drawLineStrip);
DECLARE_NIF_FUNC(drawLineBezier);
DECLARE_NIF_FUNC(drawCircle);
DECLARE_NIF_FUNC(drawCircleSector);
DECLARE_NIF_FUNC(drawCircleSectorLines);
DECLARE_NIF_FUNC(drawCircleGradient);
DECLARE_NIF_FUNC(drawCircleV);
DECLARE_NIF_FUNC(drawCircleLines);
DECLARE_NIF_FUNC(drawCircleLinesV);
DECLARE_NIF_FUNC(drawEllipse);
DECLARE_NIF_FUNC(drawEllipseLines);
DECLARE_NIF_FUNC(drawRing);
DECLARE_NIF_FUNC(drawRingLines);
DECLARE_NIF_FUNC(drawRectangle);
DECLARE_NIF_FUNC(drawRectangleV);
DECLARE_NIF_FUNC(drawRectangleRec);
DECLARE_NIF_FUNC(drawRectanglePro);
DECLARE_NIF_FUNC(drawRectangleGradientV);
DECLARE_NIF_FUNC(drawRectangleGradientH);
DECLARE_NIF_FUNC(drawRectangleGradientEx);
DECLARE_NIF_FUNC(drawRectangleLines);
DECLARE_NIF_FUNC(drawRectangleLinesEx);
DECLARE_NIF_FUNC(drawRectangleRounded);
DECLARE_NIF_FUNC(drawRectangleRoundedLines);
DECLARE_NIF_FUNC(drawTriangle);
DECLARE_NIF_FUNC(drawTriangleLines);
DECLARE_NIF_FUNC(drawTriangleFan);
DECLARE_NIF_FUNC(drawTriangleStrip);
DECLARE_NIF_FUNC(drawPoly);
DECLARE_NIF_FUNC(drawPolyLines);
DECLARE_NIF_FUNC(drawPolyLinesEx);

// Splines drawing functions
DECLARE_NIF_FUNC(drawSplineLinear);
DECLARE_NIF_FUNC(drawSplineBasis);
DECLARE_NIF_FUNC(drawSplineCatmullRom);
DECLARE_NIF_FUNC(drawSplineBezierQuadratic);
DECLARE_NIF_FUNC(drawSplineBezierCubic);
DECLARE_NIF_FUNC(drawSplineSegmentLinear);
DECLARE_NIF_FUNC(drawSplineSegmentBasis);
DECLARE_NIF_FUNC(drawSplineSegmentCatmullRom);
DECLARE_NIF_FUNC(drawSplineSegmentBezierQuadratic);
DECLARE_NIF_FUNC(drawSplineSegmentBezierCubic);

// Spline segment point evaluation functions, for a given t [0.0f .. 1.0f]
DECLARE_NIF_FUNC(getSplinePointLinear);
DECLARE_NIF_FUNC(getSplinePointBasis);
DECLARE_NIF_FUNC(getSplinePointCatmullRom);
DECLARE_NIF_FUNC(getSplinePointBezierQuad);
DECLARE_NIF_FUNC(getSplinePointBezierCubic);

// Basic shapes collision detection functions
DECLARE_NIF_FUNC(checkCollisionRecs);
DECLARE_NIF_FUNC(checkCollisionCircles);
DECLARE_NIF_FUNC(checkCollisionCircleRec);
DECLARE_NIF_FUNC(checkCollisionPointRec);
DECLARE_NIF_FUNC(checkCollisionPointCircle);
DECLARE_NIF_FUNC(checkCollisionPointTriangle);
DECLARE_NIF_FUNC(checkCollisionPointPoly);
DECLARE_NIF_FUNC(checkCollisionLines);
DECLARE_NIF_FUNC(checkCollisionPointLine);
DECLARE_NIF_FUNC(getCollisionRec);

/// -----------------------------------------------------------------------------------
///
/// module: rtextures

// Image loading functions
// NOTE: These functions do not require GPU access
DECLARE_NIF_FUNC(loadImage);
DECLARE_NIF_FUNC(loadImageRaw);
DECLARE_NIF_FUNC(loadImageSvg);
DECLARE_NIF_FUNC(loadImageAnim);
DECLARE_NIF_FUNC(loadImageFromMemory);
DECLARE_NIF_FUNC(loadImageFromTexture);
DECLARE_NIF_FUNC(loadImageFromScreen);
DECLARE_NIF_FUNC(isImageReady);
DECLARE_NIF_FUNC(unloadImage);
DECLARE_NIF_FUNC(exportImage);
DECLARE_NIF_FUNC(exportImageToMemory);
DECLARE_NIF_FUNC(exportImageAsCode);

// Image generation functions
DECLARE_NIF_FUNC(genImageColor);
DECLARE_NIF_FUNC(genImageGradientLinear);
DECLARE_NIF_FUNC(genImageGradientRadial);
DECLARE_NIF_FUNC(genImageGradientSquare);
DECLARE_NIF_FUNC(genImageChecked);
DECLARE_NIF_FUNC(genImageWhiteNoise);
DECLARE_NIF_FUNC(genImagePerlinNoise);
DECLARE_NIF_FUNC(genImageCellular);
DECLARE_NIF_FUNC(genImageText);

// Image manipulation functions
DECLARE_NIF_FUNC(imageCopy);
DECLARE_NIF_FUNC(imageFromImage);
DECLARE_NIF_FUNC(imageText);
DECLARE_NIF_FUNC(imageTextEx);
DECLARE_NIF_FUNC(imageFormat);
DECLARE_NIF_FUNC(imageToPOT);
DECLARE_NIF_FUNC(imageCrop);
DECLARE_NIF_FUNC(imageAlphaCrop);
DECLARE_NIF_FUNC(imageAlphaClear);
DECLARE_NIF_FUNC(imageAlphaMask);
DECLARE_NIF_FUNC(imageAlphaPremultiply);
DECLARE_NIF_FUNC(imageBlurGaussian);
DECLARE_NIF_FUNC(imageResize);
DECLARE_NIF_FUNC(imageResizeNN);
DECLARE_NIF_FUNC(imageResizeCanvas);
DECLARE_NIF_FUNC(imageMipmaps);
DECLARE_NIF_FUNC(imageDither);
DECLARE_NIF_FUNC(imageFlipVertical);
DECLARE_NIF_FUNC(imageFlipHorizontal);
DECLARE_NIF_FUNC(imageRotate);
DECLARE_NIF_FUNC(imageRotateCW);
DECLARE_NIF_FUNC(imageRotateCCW);
DECLARE_NIF_FUNC(imageColorTint);
DECLARE_NIF_FUNC(imageColorInvert);
DECLARE_NIF_FUNC(imageColorGrayscale);
DECLARE_NIF_FUNC(imageColorContrast);
DECLARE_NIF_FUNC(imageColorBrightness);
DECLARE_NIF_FUNC(imageColorReplace);
DECLARE_NIF_FUNC(loadImageColors);
DECLARE_NIF_FUNC(loadImagePalette);
DECLARE_NIF_FUNC(unloadImageColors);
DECLARE_NIF_FUNC(unloadImagePalette);
DECLARE_NIF_FUNC(getImageAlphaBorder);
DECLARE_NIF_FUNC(getImageColor);

// Image drawing functions
// NOTE: Image software-rendering functions (CPU)
DECLARE_NIF_FUNC(imageClearBackground);
DECLARE_NIF_FUNC(imageDrawPixel);
DECLARE_NIF_FUNC(imageDrawPixelV);
DECLARE_NIF_FUNC(imageDrawLine);
DECLARE_NIF_FUNC(imageDrawLineV);
DECLARE_NIF_FUNC(imageDrawCircle);
DECLARE_NIF_FUNC(imageDrawCircleV);
DECLARE_NIF_FUNC(imageDrawCircleLines);
DECLARE_NIF_FUNC(imageDrawCircleLinesV);
DECLARE_NIF_FUNC(imageDrawRectangle);
DECLARE_NIF_FUNC(imageDrawRectangleV);
DECLARE_NIF_FUNC(imageDrawRectangleRec);
DECLARE_NIF_FUNC(imageDrawRectangleLines);
DECLARE_NIF_FUNC(imageDraw);
DECLARE_NIF_FUNC(imageDrawText);
DECLARE_NIF_FUNC(imageDrawTextEx);

// Texture loading functions
// NOTE: These functions require GPU access
DECLARE_NIF_FUNC(loadTexture);
DECLARE_NIF_FUNC(loadTextureFromImage);
DECLARE_NIF_FUNC(loadTextureCubemap);
DECLARE_NIF_FUNC(loadRenderTexture);
DECLARE_NIF_FUNC(isTextureReady);
DECLARE_NIF_FUNC(unloadTexture);
DECLARE_NIF_FUNC(isRenderTextureReady);
DECLARE_NIF_FUNC(unloadRenderTexture);
DECLARE_NIF_FUNC(updateTexture);
DECLARE_NIF_FUNC(updateTextureRec);

// Texture configuration functions
DECLARE_NIF_FUNC(genTextureMipmaps);
DECLARE_NIF_FUNC(setTextureFilter);
DECLARE_NIF_FUNC(setTextureWrap);

// Texture drawing functions
DECLARE_NIF_FUNC(drawTexture);
DECLARE_NIF_FUNC(drawTextureV);
DECLARE_NIF_FUNC(drawTextureEx);
DECLARE_NIF_FUNC(drawTextureRec);
DECLARE_NIF_FUNC(drawTexturePro);
DECLARE_NIF_FUNC(drawTextureNPatch);

// Color/pixel related functions
DECLARE_NIF_FUNC(fade);
DECLARE_NIF_FUNC(colorToInt);
DECLARE_NIF_FUNC(colorNormalize);
DECLARE_NIF_FUNC(colorFromNormalized);
DECLARE_NIF_FUNC(colorToHSV);
DECLARE_NIF_FUNC(colorFromHSV);
DECLARE_NIF_FUNC(colorTint);
DECLARE_NIF_FUNC(colorBrightness);
DECLARE_NIF_FUNC(colorContrast);
DECLARE_NIF_FUNC(colorAlpha);
DECLARE_NIF_FUNC(colorAlphaBlend);
DECLARE_NIF_FUNC(getColor);
DECLARE_NIF_FUNC(getPixelColor);
DECLARE_NIF_FUNC(setPixelColor);
DECLARE_NIF_FUNC(getPixelDataSize);

/// -----------------------------------------------------------------------------------
///
/// module: rtext

// Font loading/unloading functions
DECLARE_NIF_FUNC(getFontDefault);
DECLARE_NIF_FUNC(loadFont);
DECLARE_NIF_FUNC(loadFontEx);
DECLARE_NIF_FUNC(loadFontFromImage);
DECLARE_NIF_FUNC(loadFontFromMemory);
DECLARE_NIF_FUNC(isFontReady);
DECLARE_NIF_FUNC(loadFontData);
DECLARE_NIF_FUNC(genImageFontAtlas);
DECLARE_NIF_FUNC(unloadFontData);
DECLARE_NIF_FUNC(unloadFont);
DECLARE_NIF_FUNC(exportFontAsCode);

// Text drawing functions
DECLARE_NIF_FUNC(drawFPS);
DECLARE_NIF_FUNC(drawText);
DECLARE_NIF_FUNC(drawTextEx);
DECLARE_NIF_FUNC(drawTextPro);
DECLARE_NIF_FUNC(drawTextCodepoint);
DECLARE_NIF_FUNC(drawTextCodepoints);

// Text font info functions
DECLARE_NIF_FUNC(setTextLineSpacing);
DECLARE_NIF_FUNC(measureText);
DECLARE_NIF_FUNC(measureTextEx);
DECLARE_NIF_FUNC(getGlyphIndex);
DECLARE_NIF_FUNC(getGlyphInfo);
DECLARE_NIF_FUNC(getGlyphAtlasRec);

// Text codepoints management functions (unicode characters)
DECLARE_NIF_FUNC(loadUTF8);
DECLARE_NIF_FUNC(unloadUTF8);
DECLARE_NIF_FUNC(loadCodepoints);
DECLARE_NIF_FUNC(unloadCodepoints);
DECLARE_NIF_FUNC(getCodepointCount);
DECLARE_NIF_FUNC(getCodepoint);
DECLARE_NIF_FUNC(getCodepointNext);
DECLARE_NIF_FUNC(getCodepointPrevious);
DECLARE_NIF_FUNC(codepointToUTF8);

// Text strings management functions (no UTF-8 strings, only byte chars)
// NOTE: Some strings allocate memory internally for returned strings, just be careful!
DECLARE_NIF_FUNC(textCopy);
DECLARE_NIF_FUNC(textIsEqual);
DECLARE_NIF_FUNC(textLength);
DECLARE_NIF_FUNC(textFormat);
DECLARE_NIF_FUNC(textSubtext);
DECLARE_NIF_FUNC(textReplace);
DECLARE_NIF_FUNC(textInsert);
DECLARE_NIF_FUNC(textJoin);
DECLARE_NIF_FUNC(textSplit);
DECLARE_NIF_FUNC(textAppend);
DECLARE_NIF_FUNC(textFindIndex);
DECLARE_NIF_FUNC(textToUpper);
DECLARE_NIF_FUNC(textToLower);
DECLARE_NIF_FUNC(textToPascal);
DECLARE_NIF_FUNC(textToInteger);

/// -----------------------------------------------------------------------------------
///
/// module: rmodels

// Basic geometric 3D shapes drawing functions
DECLARE_NIF_FUNC(drawLine3D);
DECLARE_NIF_FUNC(drawPoint3D);
DECLARE_NIF_FUNC(drawCircle3D);
DECLARE_NIF_FUNC(drawTriangle3D);
DECLARE_NIF_FUNC(drawTriangleStrip3D);
DECLARE_NIF_FUNC(drawCube);
DECLARE_NIF_FUNC(drawCubeV);
DECLARE_NIF_FUNC(drawCubeWires);
DECLARE_NIF_FUNC(drawCubeWiresV);
DECLARE_NIF_FUNC(drawSphere);
DECLARE_NIF_FUNC(drawSphereEx);
DECLARE_NIF_FUNC(drawSphereWires);
DECLARE_NIF_FUNC(drawCylinder);
DECLARE_NIF_FUNC(drawCylinderEx);
DECLARE_NIF_FUNC(drawCylinderWires);
DECLARE_NIF_FUNC(drawCylinderWiresEx);
DECLARE_NIF_FUNC(drawCapsule);
DECLARE_NIF_FUNC(drawCapsuleWires);
DECLARE_NIF_FUNC(drawPlane);
DECLARE_NIF_FUNC(drawRay);
DECLARE_NIF_FUNC(drawGrid);

// Model management functions
DECLARE_NIF_FUNC(loadModel);
DECLARE_NIF_FUNC(loadModelFromMesh);
DECLARE_NIF_FUNC(isModelReady);
DECLARE_NIF_FUNC(unloadModel);
DECLARE_NIF_FUNC(getModelBoundingBox);

// Model drawing functions
DECLARE_NIF_FUNC(drawModel);
DECLARE_NIF_FUNC(drawModelEx);
DECLARE_NIF_FUNC(drawModelWires);
DECLARE_NIF_FUNC(drawModelWiresEx);
DECLARE_NIF_FUNC(drawBoundingBox);
DECLARE_NIF_FUNC(drawBillboard);
DECLARE_NIF_FUNC(drawBillboardRec);
DECLARE_NIF_FUNC(drawBillboardPro);

// Mesh management functions
DECLARE_NIF_FUNC(uploadMesh);
DECLARE_NIF_FUNC(updateMeshBuffer);
DECLARE_NIF_FUNC(unloadMesh);
DECLARE_NIF_FUNC(drawMesh);
DECLARE_NIF_FUNC(drawMeshInstanced);
DECLARE_NIF_FUNC(exportMesh);
DECLARE_NIF_FUNC(getMeshBoundingBox);
DECLARE_NIF_FUNC(genMeshTangents);

// Mesh generation functions
DECLARE_NIF_FUNC(genMeshPoly);
DECLARE_NIF_FUNC(genMeshPlane);
DECLARE_NIF_FUNC(genMeshCube);
DECLARE_NIF_FUNC(genMeshSphere);
DECLARE_NIF_FUNC(genMeshHemiSphere);
DECLARE_NIF_FUNC(genMeshCylinder);
DECLARE_NIF_FUNC(genMeshCone);
DECLARE_NIF_FUNC(genMeshTorus);
DECLARE_NIF_FUNC(genMeshKnot);
DECLARE_NIF_FUNC(genMeshHeightmap);
DECLARE_NIF_FUNC(genMeshCubicmap);

// Material loading/unloading functions
DECLARE_NIF_FUNC(loadMaterials);
DECLARE_NIF_FUNC(loadMaterialDefault);
DECLARE_NIF_FUNC(isMaterialReady);
DECLARE_NIF_FUNC(unloadMaterial);
DECLARE_NIF_FUNC(setMaterialTexture);
DECLARE_NIF_FUNC(setModelMeshMaterial);

// Model animations loading/unloading functions
DECLARE_NIF_FUNC(loadModelAnimations);
DECLARE_NIF_FUNC(updateModelAnimation);
DECLARE_NIF_FUNC(unloadModelAnimation);
DECLARE_NIF_FUNC(unloadModelAnimations);
DECLARE_NIF_FUNC(isModelAnimationValid);

// Collision detection functions
DECLARE_NIF_FUNC(checkCollisionSpheres);
DECLARE_NIF_FUNC(checkCollisionBoxes);
DECLARE_NIF_FUNC(checkCollisionBoxSphere);
DECLARE_NIF_FUNC(getRayCollisionSphere);
DECLARE_NIF_FUNC(getRayCollisionBox);
DECLARE_NIF_FUNC(getRayCollisionMesh);
DECLARE_NIF_FUNC(getRayCollisionTriangle);
DECLARE_NIF_FUNC(getRayCollisionQuad);

/// -----------------------------------------------------------------------------------
///
/// module: raudio

// Audio device management functions
DECLARE_NIF_FUNC(initAudioDevice);
DECLARE_NIF_FUNC(closeAudioDevice);
DECLARE_NIF_FUNC(isAudioDeviceReady);
DECLARE_NIF_FUNC(setMasterVolume);
DECLARE_NIF_FUNC(getMasterVolume);

// Wave/Sound loading/unloading functions
DECLARE_NIF_FUNC(loadWave);
DECLARE_NIF_FUNC(loadWaveFromMemory);
DECLARE_NIF_FUNC(isWaveReady);
DECLARE_NIF_FUNC(loadSound);
DECLARE_NIF_FUNC(loadSoundFromWave);
DECLARE_NIF_FUNC(loadSoundAlias);
DECLARE_NIF_FUNC(isSoundReady);
DECLARE_NIF_FUNC(updateSound);
DECLARE_NIF_FUNC(unloadWave);
DECLARE_NIF_FUNC(unloadSound);
DECLARE_NIF_FUNC(unloadSoundAlias);
DECLARE_NIF_FUNC(exportWave);
DECLARE_NIF_FUNC(exportWaveAsCode);

// Wave/Sound management functions
DECLARE_NIF_FUNC(playSound);
DECLARE_NIF_FUNC(stopSound);
DECLARE_NIF_FUNC(pauseSound);
DECLARE_NIF_FUNC(resumeSound);
DECLARE_NIF_FUNC(isSoundPlaying);
DECLARE_NIF_FUNC(setSoundVolume);
DECLARE_NIF_FUNC(setSoundPitch);
DECLARE_NIF_FUNC(setSoundPan);
DECLARE_NIF_FUNC(waveCopy);
DECLARE_NIF_FUNC(waveCrop);
DECLARE_NIF_FUNC(waveFormat);
DECLARE_NIF_FUNC(loadWaveSamples);
DECLARE_NIF_FUNC(unloadWaveSamples);

// Music management functions
DECLARE_NIF_FUNC(loadMusicStream);
DECLARE_NIF_FUNC(loadMusicStreamFromMemory);
DECLARE_NIF_FUNC(isMusicReady);
DECLARE_NIF_FUNC(unloadMusicStream);
DECLARE_NIF_FUNC(playMusicStream);
DECLARE_NIF_FUNC(isMusicStreamPlaying);
DECLARE_NIF_FUNC(updateMusicStream);
DECLARE_NIF_FUNC(stopMusicStream);
DECLARE_NIF_FUNC(pauseMusicStream);
DECLARE_NIF_FUNC(resumeMusicStream);
DECLARE_NIF_FUNC(seekMusicStream);
DECLARE_NIF_FUNC(setMusicVolume);
DECLARE_NIF_FUNC(setMusicPitch);
DECLARE_NIF_FUNC(setMusicPan);
DECLARE_NIF_FUNC(getMusicTimeLength);
DECLARE_NIF_FUNC(getMusicTimePlayed);

// AudioStream management functions
DECLARE_NIF_FUNC(loadAudioStream);
DECLARE_NIF_FUNC(isAudioStreamReady);
DECLARE_NIF_FUNC(unloadAudioStream);
DECLARE_NIF_FUNC(updateAudioStream);
DECLARE_NIF_FUNC(isAudioStreamProcessed);
DECLARE_NIF_FUNC(playAudioStream);
DECLARE_NIF_FUNC(pauseAudioStream);
DECLARE_NIF_FUNC(resumeAudioStream);
DECLARE_NIF_FUNC(isAudioStreamPlaying);
DECLARE_NIF_FUNC(stopAudioStream);
DECLARE_NIF_FUNC(setAudioStreamVolume);
DECLARE_NIF_FUNC(setAudioStreamPitch);
DECLARE_NIF_FUNC(setAudioStreamPan);
DECLARE_NIF_FUNC(setAudioStreamBufferSizeDefault);
DECLARE_NIF_FUNC(setAudioStreamCallback);

DECLARE_NIF_FUNC(attachAudioStreamProcessor);
DECLARE_NIF_FUNC(detachAudioStreamProcessor);

DECLARE_NIF_FUNC(attachAudioMixedProcessor);
DECLARE_NIF_FUNC(detachAudioMixedProcessor);