PROJECT = erlaylib
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

BUILD_DEPS = raylib
dep_raylib = git-subfolder https://github.com/raysan5/raylib/ 5.0 src
PLATFORM=PLATFORM_DESKTOP
RAYLIB_PATH = "./.erlang.mk/git-subfolder/raylib/src/"

include erlang.mk


CFLAGS += -I$(RAYLIB_PATH)
LDFLAGS += -L$(RAYLIB_PATH) -lraylib -lm -lGL -lpthread -ldl -lrt -lX11