{-# LANGUAGE OverloadedStrings #-}

module Generator.CodeGen.Application.Makefile where
import Command.Types
import Generator.Makefile.AST
import System.FilePath
import qualified Data.Text as T
import Configuration.Configuration
import qualified Data.Map as M
import qualified Data.Set as S


genMakefile :: TerminaConfig -> BasicBlocksProject -> Makefile
genMakefile params bbProject =
    Makefile [
        MakeBlock [
            MVariable MSimple "R" [MFragment "$(shell pwd)"]
        ],
        MakeBlock [
            MVariable MSimple "MKDIR" [MFragment "mkdir"]
        ],
        MakeBlock [
            MVariable MSimple "PROJECT_NAME" [MFragment . T.unpack . name $ params],
            MVariable MSimple "PLATFORM" [MFragment . T.unpack . platform $ params],
            MVariable MSimple "TERMINA_OSAL_DIR" [MFragment "/opt/termina-osal"]
        ],
        MakeBlock [
            MInclude True $ "$(TERMINA_OSAL_DIR)" </> "platform" </> "$(PLATFORM)" </> "platform" <.> "mk"
        ],
        MakeBlock [
            MVariable MSimple "TARGET_DIR_NAME" [MFragment "$R/bin"],
            MVariable MSimple "TARGET" [MFragment "$(TARGET_DIR_NAME)/$(PROJECT_NAME)"]
        ],
        MakeBlock [
            MVariable MAppend "INCLUDE_DIRS" [MFragment "$R/include"]
        ],
        MakeBlock $ 
            MVariable MAppend "SRCS" [MFunction "wildcard" [MFragment "$R/*.c"]] :
            map (\srcFolder -> MVariable MAppend "SRCS" [MFunction "wildcard" [MFragment $ "$R/src/" </> srcFolder </> "*.c"]]) (S.toList sourceFolders)
        ,
        MakeBlock [
            MVariable MAppend "OBJS" [MFunction "patsubst"
                [MFragment "%.c", MFragment "%.o", MFunction "patsubst"
                    [MFragment "%.s", MFragment "%.o", MFunction "patsubst"
                        [MFragment "$(TERMINA_OSAL_DIR)/%", MFragment "$(TARGET_DIR_NAME)/termina/%", MFunction "wildcard" [MFragment "$(OSAL_SRCS)"]]]]],
            MVariable MAppend "OBJS" [MFunction "patsubst" [MFragment "%.c", MFragment "%.o", MFunction "patsubst" [MFragment "$R/%", MFragment "$(TARGET_DIR_NAME)/%", MFragment "$(SRCS)"]]]
        ],
        MakeBlock [
            MVariable MAppend "CPPFLAGS" [MFunction "patsubst" [MFragment "%", MFragment "-I %", MFragment "$(INCLUDE_DIRS)"]]
        ],
        MakeBlock [
            MVariable MSimple "DEPS" [MFunction "patsubst" [MFragment "%.o", MFragment "%.d", MFragment "$(OBJS)"]]
        ],
        MakeBlock [
            MRule "all" ["$(TARGET)"] [],
            MRule "$(TARGET_DIR_NAME)/termina/%.o" ["$(TERMINA_OSAL_DIR)/%.s"] [
                MakeCommand False [MFragment "@echo Building $<"],
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@$(AS) $(ASMFLAGS) $(CFLAGS) $(CPPFLAGS) -MMD -MP -c -o $@ $<"]
            ],
            MRule "$(TARGET_DIR_NAME)/termina/%.o" ["$(TERMINA_OSAL_DIR)/%.c"] [
                MakeCommand False [MFragment "@echo Building $<"],
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@$(CC) $(CFLAGS) $(CPPFLAGS) -MMD -MP -c -o $@ $<"]
            ],
            MRule "$(TARGET_DIR_NAME)/%.o" ["$(R)/%.c"] [
                MakeCommand False [MFragment "@echo Building $<"],
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@$(CC) $(CFLAGS) $(CPPFLAGS) -MMD -MP -c -o $@ $<"]
            ],
            MRule "$(TARGET)" ["$(OBJS)"] [
                MakeCommand False [MFragment "@echo Linking $@"],
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@$(CC) $(CFLAGS) $(OBJS) $(LDFLAGS) -o $@"]
            ],
            MRule "clean" [] [
                MakeCommand False [MFragment "@echo Cleaning up"],
                MakeCommand False [MFragment "@$(RM) $(OBJS) $(DEPS) $(TARGET)"]
            ],
            MRule "dist-clean" ["clean"] [
                MakeCommand False [MFragment "@echo Mr. Proper is cleaning up"],
                MakeCommand False [MFragment "@$(RM) -rf $(TARGET_DIR_NAME)"]
            ],
            MInclude False "$(DEPS)"
        ]

    ]

    where

        sourceFolders = foldr (S.insert . takeDirectory) S.empty $ M.keys bbProject
