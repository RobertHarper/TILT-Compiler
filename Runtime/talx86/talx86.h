typedef struct str_internal {int size; char *elts;} *talx86_string;

/* exn.c */
exn	mkDivExn(void);
exn	mkOverflowExn(void);
exn	mkSubscriptExn(void);
