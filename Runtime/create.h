/* Uses the alloc_ptr and alloc_limit of the current thread.
   Not thread-safe if differnt threads share alloc_ptr. */

void get_alloc_limit(mem_t *alloc, mem_t *limit);
val_t get_record(ptr_t rec, int which);

const ptr_t empty_record;
ptr_t alloc_intint(int m, int n);
ptr_t alloc_intrec(int k, ptr_t rec);
ptr_t alloc_recrec(ptr_t rec1, ptr_t rec2);
ptr_t alloc_manyint(int count, int n);
ptr_t alloc_manyintrec(int count, int n, ptr_t rec);
ptr_t alloc_iarray(int count, int n);
ptr_t alloc_rarray(int count, double v);
ptr_t alloc_string(int strlen, char *str);
ptr_t alloc_record(val_t *fields, int *masks, int orig_count);

void init_iarray(ptr_t obj, int byteLen, int v);
void init_parray(ptr_t obj, int wordLen, ptr_t v);
void init_farray(ptr_t obj, int doubleLen, double v);

/* these two functions should be used with care */
ptr_t alloc_uninit_string(int strlen, char **raw);
void adjust_stringlen(ptr_t str, int newlen);
