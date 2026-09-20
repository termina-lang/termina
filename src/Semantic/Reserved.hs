-- | The names of C that a Termina identifier may not take.
--
-- The transpiler does not mangle: a Termina name reaches the generated code as
-- it is written, so it lands in the same name space as the keywords of C, the
-- identifiers of its standard library and whatever the platform declares in the
-- headers that a generated module includes. Taking one of those names produces
-- code that either does not compile or that a static analyser rejects, which is
-- how index() showed up: it is declared in <strings.h>, and <string.h> pulls
-- that header in on the platforms whose flags ask for it.
--
-- The list comes in two halves. The one here is fixed by the C standard and is
-- the same everywhere. The other one is per platform, generated from a
-- translation unit that includes <termina.h> and compiled with the compiler and
-- the flags of that platform; the generator lives in tools/reserved of
-- termina-osal and its output is the Semantic.Reserved.* modules.
module Semantic.Reserved (
    ReservedBy(..), reservedName
) where

import Core.AST (Identifier)
import Configuration.Platform (Platform(..))
import Data.Char (isUpper)
import qualified Data.Set as S

import qualified Semantic.Reserved.FreeRTOS10STM32L432XX as FreeRTOS10STM32L432XX
import qualified Semantic.Reserved.POSIXGCC as POSIXGCC
import qualified Semantic.Reserved.RTEMS5LEON3NEXYSA7 as RTEMS5LEON3NEXYSA7

-- | What holds a name, which is what the error message explains.
data ReservedBy =
    -- | A keyword of C. The generated code would not even parse.
    CKeyword
    -- | An identifier of the standard library of C, reserved whether or not
    -- the header that declares it is included.
    | CStandardLibrary
    -- | An underscore and an uppercase letter, which C keeps for the
    -- implementation at every scope (ISO/IEC 9899 7.1.3).
    | CImplementation
    -- | A name that the platform declares in the headers a generated module
    -- includes.
    | CPlatform
    -- | The name of a type of Termina that takes type arguments.
    | TerminaType
  deriving (Eq, Show)

-- | What holds a name, if anything does. The answer is the same for a top-level
-- element and for a local object, which is the whole of the rule: the C name
-- space is flat and the transpiler writes both kinds of name into it unchanged.
reservedName :: Platform -> Identifier -> Maybe ReservedBy
reservedName plt ident =
  if S.member ident terminaTypes then Just TerminaType
  else if S.member ident cKeywords then Just CKeyword
  else if S.member ident cStandardLibrary then Just CStandardLibrary
  else if underscoreUpper ident then Just CImplementation
  else if S.member ident (platformNames plt) then Just CPlatform
  else Nothing

-- | The types of Termina that take type arguments, which the type checker
-- recognises by their shape instead of reading them from the global
-- environment.
terminaTypes :: S.Set Identifier
terminaTypes = S.fromList [
    "Allocator", "Atomic", "AtomicAccess", "AtomicArray", "AtomicArrayAccess",
    "MsgQueue", "Option", "Pool", "Result", "Status"
  ]

platformNames :: Platform -> S.Set Identifier
platformNames POSIXGCC = POSIXGCC.reservedNames
platformNames RTEMS5LEON3NEXYSA7 = RTEMS5LEON3NEXYSA7.reservedNames
platformNames FreeRTOS10STM32L432XX = FreeRTOS10STM32L432XX.reservedNames
-- | The platform of the test suite links against no library at all.
platformNames TestPlatform = S.empty

-- | An underscore followed by an uppercase letter is reserved for any use, at
-- any scope, and an object identifier may take that shape:
-- 'Parser.Parsing.objectIdentifierParser' accepts one leading underscore, which
-- is how the unused parameters of a handler are written. Two underscores in a
-- row are out of reach, because what follows the first one has to start at a
-- letter.
underscoreUpper :: Identifier -> Bool
underscoreUpper ('_':c:_) = isUpper c
underscoreUpper _ = False

-- | The keywords of C11, minus the ones that Termina reserves as well
-- (@const@, @struct@, @if@…), which no identifier can take anyway, and minus
-- the underscored ones (@_Atomic@, @_Bool@…), which are out of reach of the
-- lexer.
cKeywords :: S.Set Identifier
cKeywords = S.fromList [
    "auto", "break", "default", "do", "double", "extern", "float", "goto",
    "inline", "int", "long", "register", "restrict", "short", "signed",
    "sizeof", "static", "switch", "typedef", "union", "unsigned", "void",
    "volatile",
    -- | C23 spells these as keywords; before it they were macros of
    -- <stdalign.h>, <stdbool.h> and <threads.h>.
    "alignas", "alignof", "nullptr", "static_assert", "thread_local", "typeof"
  ]

-- | The identifiers of the standard library of C, header by header. Reserved
-- whether or not the header that declares them is included, so this is the half
-- of the list that holds on every platform.
cStandardLibrary :: S.Set Identifier
cStandardLibrary = S.fromList $ concat [
    -- | <assert.h>
    ["assert", "NDEBUG"],
    -- | <ctype.h>
    ["isalnum", "isalpha", "isblank", "iscntrl", "isdigit", "isgraph",
     "islower", "isprint", "ispunct", "isspace", "isupper", "isxdigit",
     "tolower", "toupper"],
    -- | <errno.h>, the three codes the standard fixes
    ["errno", "EDOM", "EILSEQ", "ERANGE"],
    -- | <fenv.h>
    ["fenv_t", "femode_t", "fexcept_t", "feclearexcept", "fegetenv",
     "fegetexceptflag", "fegetmode", "fegetround", "feholdexcept",
     "feraiseexcept", "fesetenv", "fesetexceptflag", "fesetmode", "fesetround",
     "fetestexcept", "feupdateenv", "FE_DFL_ENV"],
    -- | <float.h> and <limits.h>
    ["CHAR_BIT", "CHAR_MAX", "CHAR_MIN", "DBL_DIG", "DBL_EPSILON", "DBL_MANT_DIG",
     "DBL_MAX", "DBL_MAX_10_EXP", "DBL_MAX_EXP", "DBL_MIN", "DBL_MIN_10_EXP",
     "DBL_MIN_EXP", "DECIMAL_DIG", "FLT_DIG", "FLT_EPSILON", "FLT_EVAL_METHOD",
     "FLT_MANT_DIG", "FLT_MAX", "FLT_MAX_10_EXP", "FLT_MAX_EXP", "FLT_MIN",
     "FLT_MIN_10_EXP", "FLT_MIN_EXP", "FLT_RADIX", "FLT_ROUNDS", "LDBL_DIG",
     "LDBL_EPSILON", "LDBL_MANT_DIG", "LDBL_MAX", "LDBL_MAX_10_EXP",
     "LDBL_MAX_EXP", "LDBL_MIN", "LDBL_MIN_10_EXP", "LDBL_MIN_EXP", "LLONG_MAX",
     "LLONG_MIN", "LONG_MAX", "LONG_MIN", "MB_LEN_MAX", "SCHAR_MAX",
     "SCHAR_MIN", "SHRT_MAX", "SHRT_MIN", "UCHAR_MAX", "ULLONG_MAX",
     "ULONG_MAX", "USHRT_MAX"],
    -- | <inttypes.h>
    ["imaxabs", "imaxdiv", "imaxdiv_t", "strtoimax", "strtoumax", "wcstoimax",
     "wcstoumax"],
    -- | <iso646.h>, which spells the operators of C as macros
    ["and", "and_eq", "bitand", "bitor", "compl", "not", "not_eq", "or",
     "or_eq", "xor", "xor_eq"],
    -- | <locale.h>
    ["lconv", "localeconv", "setlocale", "LC_ALL", "LC_COLLATE", "LC_CTYPE",
     "LC_MONETARY", "LC_NUMERIC", "LC_TIME"],
    -- | <math.h>, with the float and long double flavours of every function
    mathNames,
    -- | <setjmp.h>
    ["jmp_buf", "longjmp", "setjmp"],
    -- | <signal.h>
    ["raise", "sig_atomic_t", "signal", "SIGABRT", "SIGFPE", "SIGILL",
     "SIGINT", "SIGSEGV", "SIGTERM", "SIG_DFL", "SIG_ERR", "SIG_IGN"],
    -- | <stdarg.h>
    ["va_arg", "va_copy", "va_end", "va_list", "va_start"],
    -- | <stdatomic.h>
    ["kill_dependency", "memory_order"],
    -- | <stddef.h>
    ["max_align_t", "NULL", "offsetof", "ptrdiff_t", "size_t", "wchar_t"],
    -- | <stdint.h>
    ["intmax_t", "intptr_t", "uintmax_t", "uintptr_t",
     "PTRDIFF_MAX", "PTRDIFF_MIN", "SIZE_MAX", "WCHAR_MAX", "WCHAR_MIN",
     "WINT_MAX", "WINT_MIN"] ++ sizedTypes ++ sizedLimits,
    -- | <stdio.h>
    ["BUFSIZ", "clearerr", "EOF", "fclose", "feof", "ferror", "fflush", "fgetc",
     "fgetpos", "fgets", "FILE", "FILENAME_MAX", "fopen", "FOPEN_MAX", "fpos_t",
     "fprintf", "fputc", "fputs", "fread", "freopen", "fscanf", "fseek",
     "fsetpos", "ftell", "fwrite", "getc", "getchar", "gets", "L_tmpnam",
     "perror", "printf", "putc", "putchar", "puts", "remove", "rename",
     "rewind", "scanf", "SEEK_CUR", "SEEK_END", "SEEK_SET", "setbuf", "setvbuf",
     "snprintf", "sprintf", "sscanf", "stderr", "stdin", "stdout", "tmpfile",
     "TMP_MAX", "tmpnam", "ungetc", "vfprintf", "vfscanf", "vprintf", "vscanf",
     "vsnprintf", "vsprintf", "vsscanf"],
    -- | <stdlib.h>
    ["abort", "abs", "aligned_alloc", "at_quick_exit", "atexit", "atof", "atoi",
     "atol", "atoll", "bsearch", "calloc", "div", "div_t", "exit",
     "EXIT_FAILURE", "EXIT_SUCCESS", "free", "getenv", "labs", "ldiv", "ldiv_t",
     "llabs", "lldiv", "lldiv_t", "malloc", "mblen", "mbstowcs", "mbtowc",
     "MB_CUR_MAX", "qsort", "quick_exit", "rand", "RAND_MAX", "realloc",
     "srand", "strtod", "strtof", "strtol", "strtold", "strtoll", "strtoul",
     "strtoull", "system", "wcstombs", "wctomb"],
    -- | <string.h>
    ["memchr", "memcmp", "memcpy", "memmove", "memset", "strcat", "strchr",
     "strcmp", "strcoll", "strcpy", "strcspn", "strerror", "strlen", "strncat",
     "strncmp", "strncpy", "strpbrk", "strrchr", "strspn", "strstr", "strtok",
     "strxfrm"],
    -- | <threads.h>
    ["once_flag", "ONCE_FLAG_INIT", "TSS_DTOR_ITERATIONS"],
    -- | <time.h>
    ["asctime", "clock", "CLOCKS_PER_SEC", "clock_t", "ctime", "difftime",
     "gmtime", "localtime", "mktime", "strftime", "time", "time_t", "timespec",
     "timespec_get", "tm", "TIME_UTC"],
    -- | <uchar.h>, <wchar.h> and <wctype.h>
    ["char16_t", "char32_t", "mbstate_t", "WEOF", "wctrans_t", "wctype_t",
     "wint_t", "wcscat", "wcschr", "wcscmp", "wcscoll", "wcscpy", "wcscspn",
     "wcslen", "wcsncat", "wcsncmp", "wcsncpy", "wcspbrk", "wcsrchr", "wcsspn",
     "wcsstr", "wcstok", "wcsxfrm"]
  ]

-- | The sized integer types of <stdint.h>.
sizedTypes :: [Identifier]
sizedTypes =
  concat [
    [kind ++ width ++ "_t" | kind <- ["int", "uint"], width <- widths],
    [kind ++ "_" ++ flavour ++ width ++ "_t"
      | kind <- ["int", "uint"], flavour <- ["least", "fast"], width <- widths]
  ]

  where

    widths = ["8", "16", "32", "64"]

-- | The limits and the constant builders of the sized integer types, which
-- <stdint.h> gives as macros.
sizedLimits :: [Identifier]
sizedLimits =
  concat [
    ["INT" ++ width ++ suffix | width <- widths, suffix <- ["_MAX", "_MIN", "_C"]],
    ["UINT" ++ width ++ suffix | width <- widths, suffix <- ["_MAX", "_C"]],
    ["INT_" ++ flavour ++ width ++ suffix
      | flavour <- flavours, width <- widths, suffix <- ["_MAX", "_MIN"]],
    ["UINT_" ++ flavour ++ width ++ "_MAX" | flavour <- flavours, width <- widths],
    ["INTMAX_MAX", "INTMAX_MIN", "INTMAX_C", "UINTMAX_MAX", "UINTMAX_C",
     "INTPTR_MAX", "INTPTR_MIN", "UINTPTR_MAX",
     "SIG_ATOMIC_MAX", "SIG_ATOMIC_MIN"]
  ]

  where

    widths = ["8", "16", "32", "64"]
    flavours = ["LEAST", "FAST"]

-- | The functions of <math.h>, each of them also declared with an f and with an
-- l appended for the float and the long double flavours, plus the macros of the
-- header.
mathNames :: [Identifier]
mathNames =
  ["HUGE_VAL", "HUGE_VALF", "HUGE_VALL", "INFINITY", "MATH_ERREXCEPT",
   "MATH_ERRNO", "math_errhandling", "NAN"]
  ++ concat [[fn, fn ++ "f", fn ++ "l"] | fn <- functions]

  where

    functions = [
      "acos", "acosh", "asin", "asinh", "atan", "atan2", "atanh", "cbrt",
      "ceil", "copysign", "cos", "cosh", "erf", "erfc", "exp", "exp2", "expm1",
      "fabs", "fdim", "floor", "fma", "fmax", "fmin", "fmod", "frexp", "hypot",
      "ilogb", "ldexp", "lgamma", "llrint", "llround", "log", "log10", "log1p",
      "log2", "logb", "lrint", "lround", "modf", "nan", "nearbyint",
      "nextafter", "nexttoward", "pow", "remainder", "remquo", "rint", "round",
      "scalbln", "scalbn", "sin", "sinh", "sqrt", "tan", "tanh", "tgamma",
      "trunc"]
