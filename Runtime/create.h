value_t alloc_intrec(int k, value_t rec, 
		       value_t *alloc_ptr, value_t limit);


value_t alloc_intint(int m, int n,
		       value_t *alloc_ptr, value_t limit);

value_t alloc_manyint(int count, int n,
		       value_t *alloc_ptr, value_t limit);

value_t alloc_manyintrec(int count, int n, value_t rec,
		       value_t *alloc_ptr, value_t limit);

value_t alloc_iarray(int count, int n,
		       value_t *alloc_ptr, value_t limit);

value_t alloc_recrec(value_t rec1, value_t rec2,
		     value_t *alloc_ptr, value_t limit);

value_t alloc_rarray(int count, double v,
		       value_t *alloc_ptr, value_t limit);

value_t alloc_string(int strlen, char *str,
		       value_t *alloc_ptr, value_t limit);


