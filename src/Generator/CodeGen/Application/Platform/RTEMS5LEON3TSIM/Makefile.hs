{-# LANGUAGE OverloadedStrings #-} 

module Generator.CodeGen.Application.Platform.RTEMS5LEON3TSIM.Makefile where
import Command.Types
import Generator.Makefile.AST
import qualified Data.Map as M
import System.FilePath
import qualified Data.Text as T
import Modules.Modules
import ControlFlow.Architecture.Utils
import ControlFlow.Architecture.Types
import Configuration.Configuration


genMakefile :: TerminaConfig -> BasicBlocksProject -> TerminaProgArch a -> Makefile
genMakefile params bbProject progArchitecture = 
    Makefile (variables ++ [allRule] ++ moduleRules ++ glueRules ++ [targetRule]) 

    where

        appFile = appFolder params </> appFilename params

        binaryFolder = "$R" </> "bin"
        srcFolder = "$R" </> "src"
        includeFolder = "$R" </> "include"

        target = binaryFolder </> T.unpack (name params)

        variables = [
                MVariable "R" [MSubstitution "shell pwd"],
                MVariable "MKDIR" [MFragment "mkdir"],
                MVariable "PROJECT_NAME" [MFragment . T.unpack . name $ params],
                MVariable "CC" [MFragment "/opt/rtems-noel-1.0.4/bin/riscv-rtems5-gcc"],
                MVariable "CFLAGS" [
                    MFragment "-I/opt/rcc-1.3.2-gcc/sparc-gaisler-rtems5/leon3/lib/include",
                    MFragment "-I$R/include",
                    MFragment "-I/opt/termina/api",
                    MFragment "-I/opt/termina/os/rtems5/include",
                    MFragment "-I/opt/termina/platform/rtems5_leon3_tsim/include",
                    MFragment "-fmessage-length=0",
                    MFragment "-mcpu=leon3",
                    MFragment "-qbsp=leon3",
                    MFragment "-g3"
                ]
            ]

        moduleRules = map (\m -> 
            let sourceFile = qualifiedName m <.> "c"
                headerFile = qualifiedName m <.> "h"
                moduleDeps = importedModules m in
            MRule (binaryFolder </> sourceFile -<.> "o") ([srcFolder </> sourceFile, includeFolder </> headerFile] ++ map (\n -> includeFolder </> n -<.> "h") moduleDeps) [
                MakeCommand True [
                        MFragment "echo",
                        MFragment "Building ",
                        MSubstitution "<"
                ],
                MakeCommand True [
                    MSubstitution "MKDIR",
                    MFragment "-p",
                    MSubstitution "@D"
                ],
                MakeCommand True [
                    MSubstitution "CC",
                    MSubstitution "CFLAGS",
                    MFragment "-c -o $@ $<"
                ]
            ]) $ M.elems bbProject
        
        glueRules = [
                MRule (binaryFolder </> "init" <.> "o") 
                    (("$R" </> "init" <.> "c") : map (\m -> includeFolder </> m <.> "h") (getGlobDeclModules progArchitecture)) [
                    MakeCommand True [
                        MFragment "echo",
                        MFragment "Building ",
                        MSubstitution "<"
                    ],
                    MakeCommand True [
                        MSubstitution "MKDIR",
                        MFragment "-p",
                        MSubstitution "@D"
                    ],
                    MakeCommand True [
                        MSubstitution "CC",
                        MSubstitution "CFLAGS",
                        MFragment "-c -o $@ $<"
                    ]
                ],
                MRule (binaryFolder </> "main" <.> "o") 
                    (("$R" </> "main" <.> "c") : map (\m -> includeFolder </> m <.> "h") (getGlobDeclModules progArchitecture)) [
                    MakeCommand True [
                            MFragment "echo",
                            MFragment "Building ",
                            MSubstitution "<"
                    ],
                    MakeCommand True [
                        MSubstitution "MKDIR",
                        MFragment "-p",
                        MSubstitution "@D"
                    ],
                    MakeCommand True [
                        MSubstitution "CC",
                        MSubstitution "CFLAGS",
                        MFragment "-c -o $@ $<"
                    ]
                ]
            ]
        
        targetRule = MRule target (["$R" </> "init" <.> "o", binaryFolder </> "main" <.> "o"] ++ 
                    map (\m ->
                        let objPath = if qualifiedName m == appFile then
                                appFilename params <.> "o"
                            else qualifiedName m <.> "o" in
                        binaryFolder </> objPath) (M.elems bbProject)) [
                MakeCommand True [
                    MFragment "echo",
                    MFragment "Linking ",
                    MSubstitution "@"
                ],
                MakeCommand True [
                    MSubstitution "CC",
                    MFragment "-L/opt/termina/lib/rtems5_leon3_tsim",
                    MFragment "-mcpu=leon3",
                    MFragment "-qbsp=leon3",
                    MFragment "-o $@ $^",
                    MFragment "-ltermina_rtems5_leon3_tsim"
                ]
            ]
        
        allRule = MRule "all" [target] []
