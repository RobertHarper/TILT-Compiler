/* Uses the alloc_ptr and alloc_limit of the current thread.
   Not thread-safe if differnt threads share alloc_ptr. */

void get_alloc_limit(value_t **alloc, value_t **limit);
value_t get_record(value_t rec, int which);

const value_t empty_record;
value_t alloc_intint(int m, int n);
value_t alloc_intrec(int k, value_t rec);
value_t alloc_recrec(value_t rec1, value_t rec2);
value_t alloc_manyint(int count, int n);
value_t alloc_manyintrec(int count, int n, value_t rec);
value_t alloc_iarray(int count, int n);
value_t alloc_rarray(int count, double v);
value_t alloc_string(int strlen, char *str);
value_t alloc_record(value_t *fields, int *masks, int orig_count);

void init_iarray(value_t *obj, int byteLen, int v);
void init_parray(value_t *obj, int wordLen, value_t v);
void init_farray(value_t *obj, int doubleLen, double v);

/* these two functions should be used with care */
value_t alloc_uninit_string(int strlen, char **raw);
void adjust_stringlen(value_t str, int newlen);
