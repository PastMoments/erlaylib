-module(example).
-import(erlaylib, [
	initWindow/3,
	closeWindow/0,
	windowShouldClose/0,
	beginDrawing/0,
	endDrawing/0,
	clearBackground/1,
	drawText/5,
	setTargetFPS/1
]).
-export([main/1]).


main(_) ->
	initWindow(400, 400, "test"),
	setTargetFPS(60),
	loop(),
	closeWindow().
	

loop() ->
	case windowShouldClose() of
		true -> void;
		false -> 
			beginDrawing(),
			clearBackground({245, 245, 245, 255}),
			drawText("test", 190, 200, 20, {200, 200, 200, 255 }),
			endDrawing(),
			loop()
end.