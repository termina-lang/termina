{-# LANGUAGE OverloadedStrings #-}

module Generator.CodeGen.Application.Makefile where
import Command.Types
import Command.Extern
import Generator.Makefile.AST
import System.FilePath
import qualified Data.Text as T
import Configuration.Configuration
import qualified Data.Map.Strict as M
import qualified Data.Set as S


genMakefile :: TerminaConfig -> LoweredProject -> Maybe ExternBuild -> Makefile
genMakefile params loweredProject mExtern =
    Makefile $ [
        MakeBlock [
            MVariable MSimple "R" [MFragment "$(shell pwd)"]
        ],
        MakeBlock [
            MVariable MSimple "MKDIR" [MFragment "mkdir"]
        ],
        MakeBlock [
            MVariable MSimple "PROJECT_NAME" [MFragment . T.unpack . name $ params],
            MVariable MSimple "PLATFORM" [MFragment . T.unpack . platform $ params],
            MVariable MSimple "TERMINA_OSAL_DIR" [MFragment "/opt/termina-osal"],
            MVariable MSimple "TERMINA_PROFILE"
                [MFragment (case profile params of { Debug -> "debug"; Release -> "release" })]
        ],
        MakeBlock [
            MInclude True $ "$(TERMINA_OSAL_DIR)" </> "platform" </> "$(PLATFORM)" </> "platform" <.> "mk"
        ],
        MakeBlock [
            MVariable MSimple "TARGET_DIR_NAME" [MFragment "$R/bin"],
            MVariable MSimple "TARGET" [MFragment "$(TARGET_DIR_NAME)/$(PROJECT_NAME)"]
        ],
        -- The headers of the project are reached through -iquote and not -I,
        -- since every file that includes one of them, generated or of the OSAL,
        -- does it between quotes. 
        MakeBlock [
            MVariable MAppend "PROJECT_INCLUDE_DIRS" [MFragment "$R/include"]
        ],
        MakeBlock $ 
            MVariable MAppend "SRCS" [MFunction "wildcard" [MFragment "$R/*.c"]] :
            map (\srcFolder -> MVariable MAppend "SRCS" [MFunction "wildcard" [MFragment $ "$R/src/" </> srcFolder </> "*.c"]]) (S.toList sourceFolders)
        ] ++ externVariables ++ [
        MakeBlock $ [
            MVariable MAppend "OBJS" [MFunction "patsubst"
                [MFragment "%.c", MFragment "%.o", MFunction "patsubst"
                    [MFragment "%.s", MFragment "%.o", MFunction "patsubst"
                        [MFragment "$(TERMINA_OSAL_DIR)/%", MFragment "$(TARGET_DIR_NAME)/termina/%", MFunction "wildcard" [MFragment "$(OSAL_SRCS)"]]]]],
            MVariable MAppend "OBJS" [MFunction "patsubst" [MFragment "%.c", MFragment "%.o", MFunction "patsubst" [MFragment "$R/%", MFragment "$(TARGET_DIR_NAME)/%", MFragment "$(SRCS)"]]]
        ] ++ [MVariable MAppend "OBJS" [MFragment "$(EXTERN_OBJ)"] | hasExtern],
        MakeBlock [
            MVariable MAppend "CPPFLAGS" [MFunction "patsubst" [MFragment "%", MFragment "-iquote %", MFragment "$(PROJECT_INCLUDE_DIRS)"]],
            MVariable MAppend "CPPFLAGS" [MFunction "patsubst" [MFragment "%", MFragment "-I %", MFragment "$(INCLUDE_DIRS)"]]
        ],
        MakeBlock [
            MVariable MSimple "DEPS" [MFunction "patsubst" [MFragment "%.o", MFragment "%.d",
                MFragment (if hasExtern then "$(OBJS) $(EXTERN_OBJS)" else "$(OBJS)")]]
        ],
        MakeBlock $ [
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
            ]
        ] ++ externRule ++ [
            MRule "clean" [] [
                MakeCommand False [MFragment "@echo Cleaning up"],
                MakeCommand False [MFragment
                    (if hasExtern then "@$(RM) $(OBJS) $(EXTERN_OBJS) $(DEPS) $(TARGET)" else "@$(RM) $(OBJS) $(DEPS) $(TARGET)")]
            ],
            MRule "dist-clean" ["clean"] [
                MakeCommand False [MFragment "@echo Mr. Proper is cleaning up"],
                MakeCommand False [MFragment "@$(RM) -rf $(TARGET_DIR_NAME)"]
            ],
            MInclude False "$(DEPS)"
        ],
        MakeBlock [
            MInclude True $ "$(TERMINA_OSAL_DIR)" </> "platform" </> "common" <.> "mk"
        ]

    ]

    where

        sourceFolders = foldr (S.insert . takeDirectory) S.empty $ M.keys loweredProject

        hasExtern = maybe False (const True) mExtern

        -- | The C of the project is linked into a single relocatable object in
        -- which only the extern functions stay global. How it is linked and
        -- checked depends on the toolchain, so the platform files of the OSAL
        -- define termina_extern_link.
        externVariables = case mExtern of
            Nothing -> []
            Just (ExternBuild srcs libs) -> [
                MakeBlock $
                    map (\src -> MVariable MAppend "EXTERN_SRCS" [MFragment $ "$R" </> externFolder </> src]) srcs
                    ++ map (\lib -> MVariable MAppend "EXTERN_LIBS" [MFragment $ "$R" </> externFolder </> lib]) libs
                    ++ [
                        MVariable MSimple "EXTERN_SYMBOLS" [MFragment . unwords $ externSymbols loweredProject],
                        MVariable MSimple "EXTERN_OBJS" [MFunction "patsubst" [MFragment "$R/%.c", MFragment "$(TARGET_DIR_NAME)/%.o", MFragment "$(EXTERN_SRCS)"]],
                        MVariable MSimple "EXTERN_OBJ" [MFragment "$(TARGET_DIR_NAME)/$(PROJECT_NAME).ext.o"]
                    ]
                ]

        externRule = [
            MRule "$(EXTERN_OBJ)" ["$(EXTERN_OBJS)", "$(EXTERN_LIBS)"] [
                MakeCommand False [MFragment "@echo Linking $@"],
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@$(call termina_extern_link,$@,$(EXTERN_OBJS),$(EXTERN_LIBS),$(EXTERN_SYMBOLS))"]
            ] | hasExtern]
