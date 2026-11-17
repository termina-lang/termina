
#include "test.h"

TERMINA__COVERAGE__PLACEMENT volatile uint8_t termina__cov_stmt__test[56U];
TERMINA__COVERAGE__PLACEMENT volatile uint8_t termina__cov_cond__test[30U][2U];

_Bool check(const uint32_t v) {
    
    termina__cov_stmt__test[0U] = 1U;

    return v > 10U;

}

uint32_t if_no_else(const uint32_t x) {
    
    termina__cov_stmt__test[1U] = 1U;

    uint32_t y = x;

    termina__cov_stmt__test[2U] = 1U;

    if (termina__cov__w(termina__cov_cond__test[0U], x > 3U)) {
        
        termina__cov_stmt__test[3U] = 1U;

        y = y + 1U;

    }

    termina__cov_stmt__test[4U] = 1U;

    return y;

}

uint32_t if_chain(const uint32_t x) {
    
    uint32_t y;

    termina__cov_stmt__test[5U] = 1U;

    if (termina__cov__w(termina__cov_cond__test[1U], x < 3U)) {
        
        termina__cov_stmt__test[6U] = 1U;

        y = 1U;

    } else if (termina__cov__w(termina__cov_cond__test[2U], x < 7U)) {
        
        termina__cov_stmt__test[8U] = 1U;

        y = 2U;

    } else {
        
        termina__cov_stmt__test[7U] = 1U;

        y = 3U;

    }

    termina__cov_stmt__test[9U] = 1U;

    return y;

}

uint32_t for_plain(const uint32_t n) {
    
    termina__cov_stmt__test[10U] = 1U;

    uint32_t acc = 0U;

    termina__cov_stmt__test[11U] = 1U;

    for (uint32_t i = 0U;
         termina__cov__w(termina__cov_cond__test[3U], i < 8U);
         i = i + 1U) {
        
        termina__cov_stmt__test[12U] = 1U;

        acc = acc + i;

    }

    termina__cov_stmt__test[13U] = 1U;

    return acc + n;

}

uint32_t for_while(const uint32_t limit) {
    
    termina__cov_stmt__test[14U] = 1U;

    uint32_t acc = 0U;

    termina__cov_stmt__test[15U] = 1U;

    for (uint32_t i = 0U;
         termina__cov__w(termina__cov_cond__test[4U],
                         termina__cov__w(termina__cov_cond__test[5U], i < 8U)
                         && termina__cov__w(termina__cov_cond__test[6U],
                                            acc < limit));
         i = i + 1U) {
        
        termina__cov_stmt__test[16U] = 1U;

        acc = acc + i;

    }

    termina__cov_stmt__test[17U] = 1U;

    return acc;

}

uint32_t match_no_default(const Mode m) {
    
    uint32_t y;

    termina__cov_stmt__test[18U] = 1U;

    if (termina__cov__w(termina__cov_cond__test[7U], m._variant == Mode__Off)) {
        
        termina__cov_stmt__test[19U] = 1U;

        y = 1U;

    } else if (termina__cov__w(termina__cov_cond__test[8U],
                               m._variant == Mode__Low)) {
        
        termina__cov_stmt__test[20U] = 1U;

        y = 2U;

    } else {
        
        termina__cov_stmt__test[21U] = 1U;

        y = 3U;

    }

    termina__cov_stmt__test[22U] = 1U;

    return y;

}

uint32_t match_default(const Mode m) {
    
    uint32_t y;

    termina__cov_stmt__test[23U] = 1U;

    if (termina__cov__w(termina__cov_cond__test[9U],
                        m._variant == Mode__High)) {
        
        termina__cov_stmt__test[24U] = 1U;

        y = 3U;

    } else {
        
        termina__cov_stmt__test[25U] = 1U;

        y = 9U;

    }

    termina__cov_stmt__test[26U] = 1U;

    return y;

}

uint32_t cond_and(const uint32_t a, const uint32_t b) {
    
    uint32_t y;

    termina__cov_stmt__test[27U] = 1U;

    if (termina__cov__w(termina__cov_cond__test[10U],
                        termina__cov__w(termina__cov_cond__test[11U], a > 1U)
                        && termina__cov__w(termina__cov_cond__test[12U],
                                           b > 2U))) {
        
        termina__cov_stmt__test[28U] = 1U;

        y = 1U;

    } else {
        
        termina__cov_stmt__test[29U] = 1U;

        y = 2U;

    }

    termina__cov_stmt__test[30U] = 1U;

    return y;

}

uint32_t elif_or(const uint32_t a, const uint32_t b) {
    
    uint32_t y;

    termina__cov_stmt__test[31U] = 1U;

    if (termina__cov__w(termina__cov_cond__test[13U], a == 0U)) {
        
        termina__cov_stmt__test[32U] = 1U;

        y = 1U;

    } else if (termina__cov__w(termina__cov_cond__test[14U],
                               termina__cov__w(termina__cov_cond__test[15U],
                                               a > 5U)
                               || termina__cov__w(termina__cov_cond__test[16U],
                                                  b > 5U))) {
        
        termina__cov_stmt__test[34U] = 1U;

        y = 2U;

    } else {
        
        termina__cov_stmt__test[33U] = 1U;

        y = 3U;

    }

    termina__cov_stmt__test[35U] = 1U;

    return y;

}

_Bool value_and(const uint32_t a, const uint32_t b) {
    
    termina__cov_stmt__test[36U] = 1U;

    _Bool both = termina__cov__w(termina__cov_cond__test[17U],
                                 termina__cov__w(termina__cov_cond__test[18U],
                                                 a > 1U)
                                 && termina__cov__w(termina__cov_cond__test[19U],
                                                    b > 2U));

    termina__cov_stmt__test[37U] = 1U;

    return both;

}

_Bool arg_or(const uint32_t a, const uint32_t b) {
    
    termina__cov_stmt__test[38U] = 1U;

    return termina__cov__w(termina__cov_cond__test[20U],
                           termina__cov__w(termina__cov_cond__test[21U],
                                           check(a))
                           || termina__cov__w(termina__cov_cond__test[22U],
                                              b == 0U));

}

uint32_t pick(const _Bool c, const uint32_t v) {
    
    uint32_t r;

    termina__cov_stmt__test[39U] = 1U;

    if (termina__cov__w(termina__cov_cond__test[23U], c)) {
        
        termina__cov_stmt__test[40U] = 1U;

        r = v;

    } else {
        
        termina__cov_stmt__test[41U] = 1U;

        r = 0U;

    }

    termina__cov_stmt__test[42U] = 1U;

    return r;

}

uint32_t call_with_and(const uint32_t a, const uint32_t b) {
    
    termina__cov_stmt__test[43U] = 1U;

    uint32_t r = pick(termina__cov__w(termina__cov_cond__test[24U],
                                      termina__cov__w(termina__cov_cond__test[25U],
                                                      a > 1U)
                                      && termina__cov__w(termina__cov_cond__test[26U],
                                                         b > 2U)), a);

    termina__cov_stmt__test[44U] = 1U;

    return r;

}

uint32_t index_check(const uint32_t buf[4U], const size_t i) {
    
    termina__cov_stmt__test[45U] = 1U;

    uint32_t x = buf[termina__check__array_index(4U, i)];

    termina__cov_stmt__test[46U] = 1U;

    uint32_t y = x + 1U;

    termina__cov_stmt__test[47U] = 1U;

    return y;

}

uint32_t negated_and(const uint32_t a, const uint32_t b) {
    
    termina__cov_stmt__test[48U] = 1U;

    uint32_t y = 0U;

    termina__cov_stmt__test[49U] = 1U;

    if (termina__cov__w(termina__cov_cond__test[27U],
                        (termina__cov__w(termina__cov_cond__test[28U], a > 1U)
                         && termina__cov__w(termina__cov_cond__test[29U],
                                            b > 2U)) == false)) {
        
        termina__cov_stmt__test[50U] = 1U;

        y = 1U;

    }

    termina__cov_stmt__test[51U] = 1U;

    return y;

}

uint32_t match_sole(const Single s) {
    
    termina__cov_stmt__test[52U] = 1U;

    uint32_t y = 0U;

    termina__cov_stmt__test[53U] = 1U;

    (void)s;

    termina__cov_stmt__test[54U] = 1U;

    y = y + 1U;

    termina__cov_stmt__test[55U] = 1U;

    return y;

}
