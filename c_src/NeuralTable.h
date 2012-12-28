#ifndef NEURALTABLE_H
#define NEURALTABLE_H

#include "erl_nif.h"
#include "neural_utils.h"
#include <string>
#include <stdio.h>
#include <string.h>
#include <unordered_map>
#include <queue>

#define BUCKET_COUNT 64
#define BUCKET_MASK (BUCKET_COUNT - 1)
#define GET_BUCKET(key) key & BUCKET_MASK
#define GET_LOCK(key) key & BUCKET_MASK
#define RECLAIM_THRESHOLD 1048576

using namespace std;

class NeuralTable;

typedef unordered_map<string, NeuralTable*> table_set;
typedef unordered_map<unsigned long int, ERL_NIF_TERM> hash_table;
typedef void (NeuralTable::*BatchFunction)(ErlNifPid pid);

class NeuralTable {
    public:
        static ERL_NIF_TERM MakeTable(ErlNifEnv *env, ERL_NIF_TERM name, ERL_NIF_TERM keypos);
        static ERL_NIF_TERM Insert(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key, ERL_NIF_TERM object);
        static ERL_NIF_TERM InsertNew(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key, ERL_NIF_TERM object);
        static ERL_NIF_TERM Delete(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key);
        static ERL_NIF_TERM Empty(ErlNifEnv *env, ERL_NIF_TERM table);
        static ERL_NIF_TERM Get(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key);
        static ERL_NIF_TERM Increment(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key, ERL_NIF_TERM ops);
        static ERL_NIF_TERM Shift(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key, ERL_NIF_TERM ops);
        static ERL_NIF_TERM Unshift(ErlNifEnv *env, ERL_NIF_TERM table, ERL_NIF_TERM key, ERL_NIF_TERM ops);
        static ERL_NIF_TERM Dump(ErlNifEnv *env, ERL_NIF_TERM table);
        static ERL_NIF_TERM Drain(ErlNifEnv *env, ERL_NIF_TERM table);
        static ERL_NIF_TERM GetKeyPosition(ErlNifEnv *env, ERL_NIF_TERM table);
        static ERL_NIF_TERM GarbageCollect(ErlNifEnv *env, ERL_NIF_TERM table);
        static ERL_NIF_TERM GarbageSize(ErlNifEnv *env, ERL_NIF_TERM table);
        static NeuralTable* GetTable(ErlNifEnv *env, ERL_NIF_TERM name);
        static void* DoGarbageCollection(void *table);
        static void* DoBatchOperations(void *table);

        void rlock(unsigned long int key) { enif_rwlock_rlock(locks[GET_LOCK(key)]); }
        void runlock(unsigned long int key) { enif_rwlock_runlock(locks[GET_LOCK(key)]); }
        void rwlock(unsigned long int key) { enif_rwlock_rwlock(locks[GET_LOCK(key)]); }
        void rwunlock(unsigned long int key) { enif_rwlock_rwunlock(locks[GET_LOCK(key)]); }

        ErlNifEnv *get_env(unsigned long int key);
        bool erase(unsigned long int key, ERL_NIF_TERM &ret);
        bool find(unsigned long int key, ERL_NIF_TERM &ret);
        void put(unsigned long int key, ERL_NIF_TERM tuple);
        void batch_dump(ErlNifPid pid);
        void batch_drain(ErlNifPid pid);
        void start_gc();
        void stop_gc();
        void start_batch();
        void stop_batch();
        void gc();
        void reclaim(unsigned long int key, ERL_NIF_TERM reclaim);
        unsigned long int garbage_size();
        void add_batch_job(ErlNifPid pid, BatchFunction fun);

    protected:
        static table_set tables;
        static bool running;

        struct BatchJob {
            ErlNifPid pid;
            BatchFunction fun;
        };

        NeuralTable(unsigned int kp);
        ~NeuralTable();

        unsigned int    garbage_cans[BUCKET_COUNT];
        hash_table      hash_buckets[BUCKET_COUNT];
        ErlNifEnv       *env_buckets[BUCKET_COUNT];
        ErlNifRWLock    *locks[BUCKET_COUNT];
        ErlNifCond      *gc_cond;
        ErlNifMutex     *gc_mutex;
        ErlNifTid       gc_tid;
        ErlNifCond      *batch_cond;
        ErlNifMutex     *batch_mutex;
        queue<BatchJob> batch_jobs;
        ErlNifTid       batch_tid;

        unsigned int key_pos;
};

#endif
