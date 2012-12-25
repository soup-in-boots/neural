#include "erl_nif.h"
#include "NeuralTable.h"
#include <stdio.h>

// Prototypes
static ERL_NIF_TERM neural_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM neural_put(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM neural_increment(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM neural_unshift(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM neural_shift(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM neural_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM neural_delete(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM neural_empty(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM neural_garbage(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM neural_dump(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"make_table", 1, neural_new},
    {"dump", 1, neural_dump},
    {"do_fetch", 2, neural_get},
    {"do_delete", 2, neural_delete},
    {"empty", 1, neural_empty},
    {"insert", 3, neural_put},
    {"do_increment", 3, neural_increment},
    {"do_unshift", 3, neural_unshift},
    {"do_shift", 3, neural_shift},
    {"garbage", 1, neural_garbage}
};

static ERL_NIF_TERM neural_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_atom(env, argv[0])) { return enif_make_badarg(env); }
    return NeuralTable::MakeTable(env, argv[0]);
}

static ERL_NIF_TERM neural_put(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_atom(env, argv[0]) || !enif_is_number(env, argv[1]) || !enif_is_tuple(env, argv[2])) {
        return enif_make_badarg(env);
    }

    return NeuralTable::Insert(env, argv[0], argv[1], argv[2]);
}

static ERL_NIF_TERM neural_increment(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_atom(env, argv[0]) || !enif_is_number(env, argv[1]) || !enif_is_list(env, argv[2])) {
        return enif_make_badarg(env);
    }

    return NeuralTable::Increment(env, argv[0], argv[1], argv[2]);
}

static ERL_NIF_TERM neural_shift(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_atom(env, argv[0]) || !enif_is_number(env, argv[1]) || !enif_is_list(env, argv[2])) {
        return enif_make_badarg(env);
    }

    return NeuralTable::Shift(env, argv[0], argv[1], argv[2]);
}

static ERL_NIF_TERM neural_unshift(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_atom(env, argv[0]) || !enif_is_number(env, argv[1]) || !enif_is_list(env, argv[2])) {
        return enif_make_badarg(env);
    }

    return NeuralTable::Unshift(env, argv[0], argv[1], argv[2]);
}

static ERL_NIF_TERM neural_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_atom(env, argv[0]) || !enif_is_number(env, argv[1])) {
        return enif_make_badarg(env);
    }

    return NeuralTable::Get(env, argv[0], argv[1]);
}

static ERL_NIF_TERM neural_delete(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_atom(env, argv[0]) || !enif_is_number(env, argv[1])) {
        return enif_make_badarg(env);
    }

    return NeuralTable::Delete(env, argv[0], argv[1]);
}

static ERL_NIF_TERM neural_empty(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_atom(env, argv[0])) {
        return enif_make_badarg(env);
    }

    return NeuralTable::Empty(env, argv[0]);
}

static ERL_NIF_TERM neural_dump(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_atom(env, argv[0])) {
        return enif_make_badarg(env);
    }

    return NeuralTable::Dump(env, argv[0]);
}

static ERL_NIF_TERM neural_garbage(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (!enif_is_atom(env, argv[0])) {
        return enif_make_badarg(env);
    }

    return NeuralTable::GarbageCollect(env, argv[0]);
}

static void neural_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in neural_handle */
    /* neural_handle* handle = (neural_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

ERL_NIF_INIT(neural, nif_funcs, &on_load, NULL, NULL, NULL);
