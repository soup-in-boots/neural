#include "NeuralTable.h"

table_set NeuralTable::tables;

NeuralTable::NeuralTable() {
    gc_curr = 0;
    for (int i = 0;  i < BUCKET_COUNT; ++i) {
        env_buckets[i] = enif_alloc_env();
        locks[i] = enif_rwlock_create("neural_table");
    }
}

NeuralTable::~NeuralTable() {
    for (int i = 0; i < BUCKET_COUNT; ++i) {
        enif_rwlock_destroy(locks[i]);
        enif_free_env(env_buckets[i]);
    }
}

ERL_NIF_TERM NeuralTable::MakeTable(ErlNifEnv *env, ERL_NIF_TERM name) {
    char *atom;
    string key;
    unsigned len = 0;

    if (!enif_is_atom(env, name)) { return enif_make_badarg(env); }
    enif_get_atom_length(env, name, &len, ERL_NIF_LATIN1);
    atom = (char*)enif_alloc(len + 1);

    enif_get_atom(env, name, atom, len + 1, ERL_NIF_LATIN1);
    key = atom;
    enif_free(atom);

    if (NeuralTable::tables.find(atom) != NeuralTable::tables.end()) { return enif_make_badarg(env); }
    NeuralTable::tables[key] = new NeuralTable();

    return enif_make_atom(env, "ok");
}

NeuralTable* NeuralTable::getTable(ErlNifEnv *env, ERL_NIF_TERM name) {
    char *atom = NULL;
    string key;
    unsigned len = 0;
    NeuralTable *ret = NULL;
    table_set::const_iterator it;

    if (!enif_is_atom(env, name)) { 
        return NULL; 
    }

    enif_get_atom_length(env, name, &len, ERL_NIF_LATIN1);
    atom = (char*)enif_alloc(len + 1);

    enif_get_atom(env, name, atom, len + 1, ERL_NIF_LATIN1);
    key = atom;
    enif_free(atom);

    it = NeuralTable::tables.find(key);
    if (it != NeuralTable::tables.end()) { 
        ret = it->second;
    }

    return ret;
}

ERL_NIF_TERM NeuralTable::Insert(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key, ERL_NIF_TERM object) {
    NeuralTable *tb;
    ERL_NIF_TERM ret, old;
    size_t entry_key = 0;

    // Grab table or bail.
    tb = getTable(env, table);
    if (tb == NULL) { 
        return enif_make_badarg(env); 
    }

    // Get key value.
    enif_get_ulong(env, key, &entry_key);

    // Lock the key.
    tb->rwlock(entry_key);

    // Attempt to lookup the value. If nonempty, increment
    // discarded term counter and return a copy of the
    // old value
    if (tb->find(entry_key, old)) {
        ++tb->garbage_cans[GET_BUCKET(entry_key)];
        ret = enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_copy(env, old));
    } else {
        ret = enif_make_atom(env, "ok");
    }
    
    // Write that shit out
    tb->put(entry_key, object);

    // Oh, and unlock the key if you would.
    tb->rwunlock(entry_key);

    return ret;
}

ERL_NIF_TERM NeuralTable::Increment(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key, ERL_NIF_TERM ops) {
    NeuralTable *tb;
    ERL_NIF_TERM ret, old;
    ERL_NIF_TERM it;
    size_t entry_key = 0;

    tb = getTable(env, table);
    if (tb == NULL) {
        return enif_make_badarg(env);
    }

    enif_get_ulong(env, key, &entry_key);

    tb->rwlock(entry_key);

    if (tb->find(entry_key, old)) {
        ERL_NIF_TERM op_cell;
        const ERL_NIF_TERM *tb_tpl;
        const ERL_NIF_TERM *op_tpl;
        ERL_NIF_TERM *new_tpl;
        ErlNifEnv *bucket_env = tb->get_env(entry_key);
        unsigned long int   pos         = 0;
        long int            incr        = 0;
        unsigned int        ops_length  = 0;
        int                 op_arity    = 0,
                            tb_arity    = 0;

        enif_get_tuple(bucket_env, old, &tb_arity, &tb_tpl);
        new_tpl = (ERL_NIF_TERM*)enif_alloc(sizeof(ERL_NIF_TERM) * tb_arity);
        memcpy(new_tpl, tb_tpl, sizeof(ERL_NIF_TERM) * tb_arity);

        it = ops;
        ret = enif_make_list(env, 0);
        while(!enif_is_empty_list(env, it)) {
            enif_get_list_cell(env, it, &op_cell, &it);
            long int value = 0;
            enif_get_tuple(env, op_cell, &op_arity, &op_tpl);
            enif_get_ulong(env, op_tpl[0], &pos);
            enif_get_long(env, op_tpl[1], &incr);

            if (pos <= 0 || pos > tb_arity) {
                ret = enif_make_badarg(env);
                goto bailout;
            }

            if (!enif_is_number(bucket_env, new_tpl[pos - 1])) {
                ret = enif_make_badarg(env);
                goto bailout;
            }

            enif_get_long(env, new_tpl[pos - 1], &value);
            new_tpl[pos - 1] = enif_make_long(bucket_env, value + incr);
            ret = enif_make_list_cell(env, enif_make_copy(env, new_tpl[pos - 1]), ret);
        }

        tb->put(entry_key, enif_make_tuple_from_array(bucket_env, new_tpl, tb_arity));

bailout:
        enif_free(new_tpl);
        tb->rwunlock(entry_key);
    }

    return ret;
}

ERL_NIF_TERM NeuralTable::Delete(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key) {
    NeuralTable *tb;
    ERL_NIF_TERM val, ret;
    unsigned long int entry_key;

    tb = getTable(env, table);
    if (tb == NULL) { return enif_make_badarg(env); }

    enif_get_ulong(env, key, &entry_key);

    tb->rwlock(entry_key);

    if (tb->erase(entry_key, val)) {
        ret = enif_make_copy(env, val);
        ++tb->garbage_cans[GET_BUCKET(entry_key)];
    } else {
        ret = enif_make_atom(env, "undefined");
    }

    tb->rwunlock(entry_key);

    return ret;
}

ERL_NIF_TERM NeuralTable::Get(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key) {
    NeuralTable *tb;
    ERL_NIF_TERM ret, val;
    unsigned long int entry_key;

    // Acquire table handle, or quit if the table doesn't exist.
    tb = getTable(env, table);
    if (tb == NULL) { return enif_make_badarg(env); }

    // Get key value
    enif_get_ulong(env, key, &entry_key);

    // Lock the key
    tb->rlock(entry_key);

    // Read current value
    if (!tb->find(entry_key, val)) {
        ret = enif_make_atom(env, "undefined");
    } else {
        ret = enif_make_copy(env, val);
    }

    tb->runlock(entry_key);

    return ret;
}

ERL_NIF_TERM NeuralTable::Dump(ErlNifEnv *env, ERL_NIF_TERM table) {
    NeuralTable *tb = getTable(env, table);
    ERL_NIF_TERM ret = enif_make_list(env, 0);

    for (int i = 0; i < BUCKET_COUNT; ++i) {
        for (hash_table::iterator it = tb->hash_buckets[i].begin(); it != tb->hash_buckets[i].end(); ++it) {
            ret = enif_make_list_cell(env, enif_make_copy(env, it->second), ret);
        }
    }

    return ret;
}

ERL_NIF_TERM NeuralTable::GarbageCollect(ErlNifEnv *env, ERL_NIF_TERM table) {
    NeuralTable *tb = getTable(env, table);
    if (tb == NULL) { return enif_make_badarg(env); }

    tb->gc();

    return enif_make_atom(env, "ok");
}

void NeuralTable::put(unsigned long int key, ERL_NIF_TERM tuple) {
    ErlNifEnv *env = get_env(key);
    hash_buckets[GET_BUCKET(key)][key] = enif_make_copy(env, tuple);
}

ErlNifEnv* NeuralTable::get_env(unsigned long int key) {
    return env_buckets[GET_BUCKET(key)];
}

bool NeuralTable::find(unsigned long int key, ERL_NIF_TERM &ret) {
    hash_table *bucket = &hash_buckets[GET_BUCKET(key)];
    hash_table::iterator it = bucket->find(key);
    if (bucket->end() == it) {
        return false;
    } else {
        ret = it->second;
        return true;
    }
}

bool NeuralTable::erase(unsigned long int key, ERL_NIF_TERM &val) {
    hash_table *bucket = &hash_buckets[GET_BUCKET(key)];
    hash_table::iterator it = bucket->find(key);
    bool ret = false;
    if (it != bucket->end()) {
        ret = true;
        val = it->second;
        bucket->erase(it);
    }
    return ret;
}

void NeuralTable::gc() {
    ErlNifEnv *fresh    = NULL,
              *old      = NULL;
    hash_table *bucket  = NULL;
    hash_table::iterator it;

    bucket = &hash_buckets[gc_curr];
    old = env_buckets[gc_curr];
    fresh = enif_alloc_env();

    enif_rwlock_rwlock(locks[gc_curr]);
    for  (it = bucket->begin(); it != bucket->end(); ++it) {
        it->second = enif_make_copy(fresh, it->second);
    }

    garbage_cans[gc_curr] = 0;
    env_buckets[gc_curr] = fresh;
    enif_free_env(old);
    enif_rwlock_rwunlock(locks[gc_curr]);

    if (++gc_curr >= BUCKET_COUNT) { gc_curr = 0; }
}
