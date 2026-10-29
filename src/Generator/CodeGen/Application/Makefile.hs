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


-- | The flag says whether the deployment is a unit test, which reads the test clock
genMakefile :: TerminaConfig -> LoweredProject -> Maybe ExternBuild -> Bool -> Makefile
genMakefile params loweredProject mExtern unitTest =
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
            -- | TERMINA_OSAL_DIR from the environment of make, else the
            -- default installation path.
            MVariable MConditional "TERMINA_OSAL_DIR" [MFragment "/opt/termina/osal"],
            MVariable MSimple "TERMINA_PROFILE"
                [MFragment (case profile params of { Debug -> "debug"; Release -> "release" })]
        ]] ++ testFlags ++ bufferSizeFlags ++ [
        MakeBlock [
            MInclude True $ "$(TERMINA_OSAL_DIR)" </> "platform" </> "$(PLATFORM)" </> "platform" <.> "mk"
        ],
        MakeBlock [
            MVariable MConditional "TERMINA_SHA256" [MFragment "$(if $(shell command -v sha256sum 2>/dev/null),sha256sum,shasum -a 256)"]
        ],
        -- | The compiler runs from the folder of the project, and the
        -- debugging information names its files relative to it, as the #line
        -- pragmas do, so the debugger finds them wherever the project is
        MakeBlock [
            MVariable MSimple "TERMINA_PROJECT_DIR" [MFunction "abspath" [MFragment $ "$R" </> projectRoot]],
            MVariable MAppend "CFLAGS" [MFragment "-ffile-prefix-map=$(TERMINA_PROJECT_DIR)=."]
        ],
        -- | The binary goes alone in bin, so its name cannot be the one of a
        -- folder or a file of the build, which go in obj
        MakeBlock [
            MVariable MSimple "TARGET_DIR_NAME" [MFragment "$R/bin"],
            MVariable MSimple "OBJ_DIR_NAME" [MFragment "$R/obj"],
            MVariable MSimple "TARGET" [MFragment "$(TARGET_DIR_NAME)/$(PROJECT_NAME)"]
        ],
        -- | The tools and the flags of the build, which every object depends
        -- on, so a change of profile or of platform flags rebuilds them all
        MakeBlock [
            MVariable MSimple "TERMINA_FLAGS_FILE" [MFragment "$(OBJ_DIR_NAME)/flags"],
            MVariable MRecursive "TERMINA_FLAGS" [MFragment "$(CC) $(AS) $(CFLAGS) $(CPPFLAGS) $(ASMFLAGS) $(LDFLAGS)"]
        ],
        -- The headers of the project are reached through -iquote and not -I,
        -- since every file that includes one of them, generated or of the OSAL,
        -- does it between quotes. 
        MakeBlock [
            MVariable MAppend "PROJECT_INCLUDE_DIRS" [MFragment "$R/include"]
        ],
        MakeBlock $ 
            MVariable MAppend "SRCS" [MFunction "wildcard" [MFragment "$R/*.c"]] :
            map (\srcFolder -> MVariable MAppend "SRCS" [MFunction "wildcard" [MFragment $ normalise ("$R/src/" </> srcFolder </> "*.c")]]) (S.toList sourceFolders)
        ] ++ externVariables ++ [
        MakeBlock $ [
            MVariable MAppend "OBJS" [MFunction "patsubst"
                [MFragment "%.c", MFragment "%.o", MFunction "patsubst"
                    [MFragment "%.s", MFragment "%.o", MFunction "patsubst"
                        [MFragment "$(TERMINA_OSAL_DIR)/%", MFragment "$(OBJ_DIR_NAME)/termina/%", MFunction "wildcard" [MFragment "$(OSAL_SRCS)"]]]]],
            MVariable MAppend "OBJS" [MFunction "patsubst" [MFragment "%.c", MFragment "%.o", MFunction "patsubst" [MFragment "$R/%", MFragment "$(OBJ_DIR_NAME)/%", MFragment "$(SRCS)"]]],
            MVariable MSimple "MODULE_OBJS" [MFunction "patsubst" [MFragment "$R/%.c", MFragment "$(OBJ_DIR_NAME)/%.o", MFunction "filter" [MFragment "$R/src/%", MFragment "$(SRCS)"]]]
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
            -- | A recipe that fails leaves no target that make would take as
            -- up to date
            MRule ".DELETE_ON_ERROR" [] [],
            -- | The file is rewritten only when its content changes, so its
            -- date tells make when the flags changed
            MRule ".PHONY" ["termina-flags"] [],
            MRule "$(TERMINA_FLAGS_FILE)" ["termina-flags"] [
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@if [ \"$$(cat $@ 2>/dev/null)\" != '$(subst ','\\'',$(TERMINA_FLAGS))' ]; then printf '%s\\n' '$(subst ','\\'',$(TERMINA_FLAGS))' > $@; fi"]
            ],
            MRule "$(OBJ_DIR_NAME)/termina/%.o" ["$(TERMINA_OSAL_DIR)/%.s", "$(TERMINA_FLAGS_FILE)"] [
                MakeCommand False [MFragment "@echo Building $<"],
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@cd $(TERMINA_PROJECT_DIR) && $(AS) $(ASMFLAGS) $(CFLAGS) $(CPPFLAGS) -MMD -MP -c -o $@ $<"]
            ],
            MRule "$(OBJ_DIR_NAME)/termina/%.o" ["$(TERMINA_OSAL_DIR)/%.c", "$(TERMINA_FLAGS_FILE)"] [
                MakeCommand False [MFragment "@echo Building $<"],
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@cd $(TERMINA_PROJECT_DIR) && $(CC) $(CFLAGS) $(CPPFLAGS) -MMD -MP -c -o $@ $<"]
            ],
            MRule "$(OBJ_DIR_NAME)/%.o" ["$(R)/%.c", "$(TERMINA_FLAGS_FILE)"] [
                MakeCommand False [MFragment "@echo Building $<"],
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@cd $(TERMINA_PROJECT_DIR) && $(CC) $(CFLAGS) $(TERMINA_EXTERN_CPPFLAGS) $(CPPFLAGS) -MMD -MP -c -o $@ $<"]
            ],
            -- | The SHA-256 of the binary and of each module object without
            -- their debugging information, which the build writes into
            -- provenance.json
            MRule ".PHONY" ["stripped-hashes"] [],
            MRule "stripped-hashes" ["$(TARGET)", "$(MODULE_OBJS)"] [
                MakeCommand False [MFragment "@$(RM) $(TARGET_DIR_NAME)/stripped.sha256"],
                MakeCommand False [MFragment $ unwords [
                    "@if command -v $(OBJCOPY) >/dev/null 2>&1; then",
                    "for o in $(TARGET) $(MODULE_OBJS); do $(OBJCOPY) --strip-debug $$o $$o.stripped || exit 1;",
                    "printf '%s %s\\n' $$($(TERMINA_SHA256) < $$o.stripped | cut -c 1-64) $${o#$R/}; $(RM) $$o.stripped; done > $(TARGET_DIR_NAME)/stripped.sha256;",
                    "else echo $(OBJCOPY) not found: the objects are not hashed; fi"]]
            ],
            -- | The resolved values that termina deliver writes into the flat
            -- Makefile of the deliverable, one per line. CROSS goes
            -- unexpanded, so that it keeps the variable of the toolchain.
            MRule ".PHONY" ["deliverable-info"] [],
            MRule "deliverable-info" [] $
                map (\v -> MakeCommand False [MFragment $ "$(info " ++ v ++ "=$(" ++ v ++ "))"]) deliverableVariables
                ++ [
                    MakeCommand False [MFragment "$(info CROSS=$(value CROSS))"],
                    MakeCommand False [MFragment "$(info EXPANDED_CROSS=$(CROSS))"],
                    MakeCommand False [MFragment "$(info OSAL_SRCS=$(wildcard $(OSAL_SRCS)))"],
                    MakeCommand False [MFragment "$(info CC_VERSION=$(shell $(CC) --version 2>/dev/null | head -n 1))"],
                    MakeCommand False [MFragment "$(foreach v,$(.VARIABLES),$(if $(findstring $$($(v)),$(value CROSS)),$(info VAR $(v)=$($(v)))))"]
                ],
            MRule "$(TARGET)" (["$(OBJS)", "$(TERMINA_FLAGS_FILE)"] ++ ["$R/test.json" | isTestBuild params]) ([
                MakeCommand False [MFragment "@echo Linking $@"],
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@$(CC) $(CFLAGS) $(OBJS) $(LDFLAGS) -o $@"]
            ] ++ descriptorSection)
        ] ++ externRule ++ [
            MRule "clean" [] [
                MakeCommand False [MFragment "@echo Cleaning up"],
                MakeCommand False [MFragment
                    (if hasExtern then "@$(RM) $(OBJS) $(EXTERN_OBJS) $(DEPS) $(TARGET) $(TARGET).map" else "@$(RM) $(OBJS) $(DEPS) $(TARGET) $(TARGET).map")]
            ],
            MRule "dist-clean" ["clean"] [
                MakeCommand False [MFragment "@echo Mr. Proper is cleaning up"],
                MakeCommand False [MFragment "@$(RM) -rf $(TARGET_DIR_NAME) $(OBJ_DIR_NAME) $(CPPCHECK_BUILD_DIR) $(CPPCHECK_MISRA_BUILD_DIR)"]
            ],
            MInclude False "$(DEPS)"
        ],
        MakeBlock [
            MInclude True $ "$(TERMINA_OSAL_DIR)" </> "platform" </> "common" <.> "mk"
        ]

    ]

    where

        sourceFolders = foldr (S.insert . takeDirectory) S.empty $ M.keys loweredProject

        -- | The folder of the project seen from the output folder of the
        -- deployment, which is always inside it
        projectRoot = joinPath (map (const "..") (splitDirectories (outputFolder params)))

        hasExtern = maybe False (const True) mExtern

        -- | The descriptor of a test goes into a section of the binary that is not loaded
        descriptorSection = [
            MakeCommand False [MFragment $ unwords [
                "@if command -v $(OBJCOPY) >/dev/null 2>&1; then",
                "$(OBJCOPY) --add-section .termina_test=$R/test.json $@;",
                "else echo $(OBJCOPY) not found: the descriptor is not added to the binary; fi"]]
            | isTestBuild params ]

        -- | A test builds the test runtime of the OSAL, which platform.mk adds
        testFlags = [
                MakeBlock $
                    MVariable MSimple "TERMINA_TEST" [MFragment "1"]
                    : [MVariable MAppend "CFLAGS" [MFragment "-DTERMINA__TEST__CLOCK"] | unitTest]
            | isTestBuild params ]

        deliverableVariables = [
            "R", "TERMINA_OSAL_DIR", "TERMINA_PROJECT_DIR", "CC", "AS", "LD", "OBJCOPY", "NM", "OBJDUMP",
            "TERMINA_EXTERN_LIBC", "CFLAGS", "CPPFLAGS", "ASMFLAGS", "LDFLAGS", "SRCS",
            "EXTERN_SRCS", "EXTERN_LIBS", "EXTERN_SYMBOLS", "DEPS"]

        -- | The buffer sizes of sys_print and sys_read appear in the headers of
        -- the OSAL API, so every module sees them. They belong to the project
        -- and go as flags, not in config.h, which belongs to the deployment.
        bufferSizeFlags = case
                [MVariable MAppend "CFLAGS" [MFragment $ "-DTERMINA__SYS_PRINT__OUTPUT_BUFFER_SIZE=" ++ show (sysPrintOutputBufferSize params)]
                    | sysPrintOutputBufferSize params /= defaultSysPrintOutputBufferSize]
                ++ [MVariable MAppend "CFLAGS" [MFragment $ "-DTERMINA__SYS_READ__INPUT_BUFFER_SIZE=" ++ show (sysReadInputBufferSize params)]
                    | sysReadInputBufferSize params /= defaultSysReadInputBufferSize] of
            [] -> []
            flags -> [MakeBlock flags]

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
                        MVariable MSimple "EXTERN_OBJS" [MFunction "patsubst" [MFragment "$R/%.c", MFragment "$(OBJ_DIR_NAME)/%.o", MFragment "$(EXTERN_SRCS)"]],
                        MVariable MSimple "EXTERN_OBJ" [MFragment "$(OBJ_DIR_NAME)/$(PROJECT_NAME).ext.o"]
                    ]
                ]

        externRule = concat [ [
            -- | The headers of the project come before the generated ones, for
            -- its C only
            MRule "$(EXTERN_OBJS)" ["TERMINA_EXTERN_CPPFLAGS:=-iquote $R" </> externFolder </> externHeaderFolder] [],
            -- | The Makefile holds the list of extern functions, so a new one
            -- relinks the object even when no C changed
            MRule "$(EXTERN_OBJ)" ["$(EXTERN_OBJS)", "$(EXTERN_LIBS)", "$R/Makefile", "$(TERMINA_FLAGS_FILE)"] [
                MakeCommand False [MFragment "@echo Linking $@"],
                MakeCommand False [MFragment "@$(MKDIR) -p $(@D)"],
                MakeCommand False [MFragment "@$(call termina_extern_link,$@,$(EXTERN_OBJS),$(EXTERN_LIBS),$(EXTERN_SYMBOLS))"]
            ] ] | hasExtern]
