
#include "test.h"

_Bool guarded(const uint32_t buf[4U], const size_t i) {
    
    return i < 4U && buf[i] > 0U;

}

_Bool guarded_size(const uint32_t buf[4U], const size_t i) {
    
    return i < 4U && buf[i] > 0U;

}

_Bool guarded_or(const uint32_t buf[4U], const size_t i) {
    
    return i >= 4U || buf[i] > 0U;

}

_Bool divided(const uint32_t y, const uint32_t x) {
    
    return x != 0U && (uint32_t)(y / x) > 1U;

}

_Bool divided_or(const uint32_t y, const uint32_t x) {
    
    return x == 0U || (uint32_t)(y % x) == 0U;

}

_Bool shifted(const uint32_t v, const uint32_t s) {
    
    return s < 32U && (uint32_t)(v << s) > 0U;

}
