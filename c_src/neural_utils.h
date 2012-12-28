#ifndef NEURAL_UTILS_H
#define NEURAL_UTILS_H

#include "erl_nif.h"
#define WORD_SIZE sizeof(int)

unsigned long int estimate_size(ErlNifEnv *env, ERL_NIF_TERM term);

#endif
