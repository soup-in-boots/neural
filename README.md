NEURAL: Erlang's Universal Record Adjustment Layer
------

NEURAL provides an ets-like interface to shared terms with additional support for in-place operations on stored data, such as incrementing a value, or adding/removing a value to a list.

### Usage ###

The API is similar to that of ets, but at the time is more limited in its configurability. All tables are sets and are always identified by atoms. Key position is configurable (great for use with records). As with ets, a key can be any term. The value at the key position is hashed with erlang:phash2/1 before calling to the NIF.

#### Create a table ####
Use neural:new/2

```erlang
neural:new(tuple_table, []).
neural:new(record_table, [{key_pos, 2}]).
```

#### Insert a Tuple ####
Use neural:insert/2 or neural:insert_new/2

```erlang
neural:insert(table_name, {"an element", 1}).
false = neural:insert_new(table_name, {"an element", []}).
true = neural:insert_new(table_name, {"another element", []}).
```

#### Retrieve a Tuple ####
Use neural:lookup/2

```erlang
{"an_element", 1} = neural:lookup(table_name, "an element").
undefined = neural:lookup(table_name, "no such key").
```

#### Delete a Tuple ####
Use neural:delete/2 

This function also returns the stored tuple if such a tuple is found, or undefined if it wasn't.

```erlang
{ok, {"an_element", 1}} = neural:delete(table_name, "an element").
{ok, undefined} = neural:delete(table_name, "no such key").
```

#### Update Counter ####
Use neural:increment/3

The third argument is the operation to perform. Like ets:update_counter/3, the update operation specifies a position and an increment as a tuple. The third argument can be a list of such operations, a single operation, or a integer (in which case it is treated as the value and the position is assumed to be the key position + 1.

```erlang
2 = neural:increment(table_name, "an element", 1).
3 = neural:increment(table_name, "an element", {2, 1}).
[4, 5] = neural:increment(table_name, "an_element", [{2, 1}, {2, 1}]).
```

#### Update List ####
Use neural:shift/3 to remove elements from a list

Use neural:unshift/3 to add elements to a list

##### neural:unshift/3 ######
The third argument is the operation to perform. Unlike neural:increment/3, the third argument must either be a single operation tuple or a list of operation tuples. This is because of the ambiguity of a list of values to unshift. Elements are copied from head to tail by adding to the head of the stored element.

Returns the new length of the list.

```erlang
% Results in [d, c, b, a]
4 = neural:unshift(table_name, "another element", {2, [a, b, c, d]}).
```

##### neural:shift/3 #####
The third argument is the operation to perform. An operation specifies a position to modify and a number of elements to remove. The third argument may be a list of such operations, a single operation tuple, or a single integer (in which case it assumes key position + 1). Elements are taken from the head of the stored list and appended to the head of the return list.

If the number of elements to remove is less than 0, the entire list will be removed and returned.

```erlang
[d] = neural:shift(table_name, "another element", 1).
[a, b, c] = neural:shift(table_name, "another element", -1).
```

#### Batch Operations ####
Use neural:dump/1 to read the entire contents of the table

Use neural:drain/1 to read and remove the entire contents of the table

Use neural:erase/1 to remove the entire contents of the table

Each function takes only the table name as an argument. Batch operations are executed in a separate thread, and the results are sent via message passing to the calling process. This is because calling a long-running NIF call from an erlang process can cause problems with Erlang's schedulers. Other potentially long-running calls could eventually be moved into batch threads as well.

### Garbage Collection ###
NEURAL stores terms by copying them to a process-independent environment. Most modifications to the data therein will therefore result in discarded terms. For this reason, NEURAL has to deliberately collect garbage (erlang terms are valid for the entire life of their environment).

It does this by keeping track of the approximate word size of each term that is discard, and, when the amount discarded surpasses a certain threshold, triggers the garbage collection condition. Each table has a dedicated garbage collection thread which triggers on this condition. The garbage collection thread walks through each bucket in the table, copies each bucket's terms to a new environment, and frees the old environment.
