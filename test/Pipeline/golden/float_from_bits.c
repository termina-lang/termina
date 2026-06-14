
#include "test.h"

float32_t pipeline_floats(uint32_t raw) {
    
    float32_t value = f32_from_bits(raw);

    value = value - 1.0f;

    return value;

}
