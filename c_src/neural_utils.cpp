#include "neural_utils.h"

unsigned long int estimate_size(ErlNifEnv *env, ERL_NIF_TERM term) {
    if (enif_is_atom(env, term)) {
        return WORD_SIZE;
    }

    // Treating all numbers like longs.
    if (enif_is_number(env, term)) {
        return 2 * WORD_SIZE;
    }

    if (enif_is_binary(env, term)) {
        ErlNifBinary bin;
        enif_inspect_binary(env, term, &bin);
        return bin.size + (6 * WORD_SIZE);
    }

    if (enif_is_list(env, term)) {
        unsigned long int size = 0;
        ERL_NIF_TERM it, curr;
        it = term;
        size += WORD_SIZE;
        while (!enif_is_empty_list(env, it)) {
            enif_get_list_cell(env, it, &curr, &it);
            size += estimate_size(env, curr) + WORD_SIZE;
        }
        return size;
    }

    if (enif_is_tuple(env, term)) {
        unsigned long int size = 0;
        const ERL_NIF_TERM *tpl;
        int arity;
        enif_get_tuple(env, term, &arity, &tpl);
        for (int i = 0; i < arity; ++i) {
            size += estimate_size(env, tpl[i]);
        }
        return size;
    }

    // Return 1 word by default
    return WORD_SIZE;
}


