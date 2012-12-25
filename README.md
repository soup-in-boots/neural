NEURAL: Erlang's Universal Record Adjustment Layer
------

NEURAL provides an ets-like interface to shared terms with additional support for in-place operations on stored data, such as incrementing a value, or adding/removing a value to a list.

NEURAL stores terms by copying them to a process-independent environment. Most modifications to the data therein will therefore result in discarded terms. For this reason, NEURAL has to deliberately collect garbage (erlang terms are valid for the entire life of their environment). It does this, at present, in a rather inefficient manner, using an erlang process to periodically call into the NIF to initiate the cleanup process.
