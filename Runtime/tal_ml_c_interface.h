/* Copied from tag.h */
typedef unsigned int *mem_t;  /* A memory address into the stack and heap.
				 There is not necessarily a object at the location. */
typedef unsigned int ui_t;
typedef ui_t  val_t;        /* An ML value that may or may nor be a pointer. */
typedef ui_t *ptr_t;        /* An ML value that has a pointer type.
                                It might be a small-valued constructor (<= 256),
			        points to global data (in the data segment),
			        or is in the heap(s). */

typedef struct arr_internal {int size; void *elts;} *array;

#define arraysize(a) (sizeof(a)/sizeof(*(a)))

#define PAIR(t,t1,t2,tlong,tstruct) struct tstruct { t1 first; t2 second; }; \
                                    typedef struct tstruct *tlong; \
                                    typedef ptr_t t;
                           
#define TRIPLE(t,t1,t2,t3,tlong,tstruct) struct tstruct { t1 first; t2 second; t3 third; }; \
                                         typedef struct tstruct *tlong; \
                                         typedef ptr_t t;

#define LIST(t,tlist,tlong,tstruct) struct tstruct \
                        { t car; \
			  ptr_t cdr; \
			}; \
                        typedef struct tstruct *tlong; \
                        typedef ptr_t tlist;

typedef ptr_t unit;  /* The value 256 */
typedef int bool;
typedef int posint;
typedef int word;
typedef char word8;

typedef struct str_internal {int size; char *elts;} *string;
typedef string word8array;
typedef const string word8vector;

PAIR(intpair,int,int,intpair_long,intpair_struct)
typedef intpair intpair_option;
typedef string string_option;
TRIPLE(inttriple,int,int,int,inttriple_long,inttriple_struct)
PAIR(wordpair,word,word,wordpair_long,wordpair_struct)
PAIR(intword,int,word,intword_long,intword_struct)
LIST(intword,intword_list,intword_list_long,intword_list_struct)
LIST(word,word_list,word_list_long,word_list_struct)
LIST(string,string_list,string_list_long,string_list_struct)
PAIR(string_stringlist,string,string_list,string_stringlist_long,string_stringlist_struct)
TRIPLE(string_word_stringlist,string,word,string_list,string_word_stringlist_long,string_word_stringlist_struct)


#define empty_record ((ptr_t)256)/* This is the ML unit. */

ptr_t alloc_record(val_t *fields, int count);

string alloc_string(int strlen, char *str);
string alloc_uninit_string(int strlen, char **raw);
void adjust_stringlen(string str, int newByteLen);
ptr_t alloc_manyint(int count, int v);
ptr_t alloc_intint(int a, int b);
int stringlen(string na);
char *string_get_elts(string na);
string cstring2mlstring_alloc(char *str);
char* mlstring2cstring_buffer(string mlstring, int len, char *buf);
char* mlstring2cstring_static(string mlstring);
char* mlstring2cstring_malloc(string mlstring);

/* The Basis initialization should call this code to 
 * tell the runtime about some key ML level exceptions.
 */
unit registerSubExnRuntime(ptr_t exn);
unit registerDivExnRuntime(ptr_t exn);
unit registerOvflExnRuntime(ptr_t exn);
unit registerSysErrExnRuntime(ptr_t exn);
unit registerLibFailExnRuntime(ptr_t exn);
ptr_t getSubStamp(void);
ptr_t getDivStamp(void);
ptr_t getOvflStamp(void);
ptr_t getSysErrStamp(void);
ptr_t getLibFailStamp(void);
ptr_t mkExn(string exnname, ptr_t exnstamp, val_t exnarg);
ptr_t mkDivExn(void);
ptr_t mkOverflowExn(void);
ptr_t mkSubscriptExn(void);
ptr_t mkSysErrExn(string msg, int isSome, int e);
string exnNameRuntime(ptr_t exn);
char *exnCNameRuntime(ptr_t exn);
string exnMessageRuntime(ptr_t exn);
ptr_t cons_rec_alloc(ptr_t car, ptr_t list);
ptr_t cons_val_alloc(val_t val, ptr_t list);
ptr_t stringpair_ctoml_alloc(char* a, char* b);

/* Translate a list of ml strings to an array of c strings */
char** string_list_to_array_malloc(string_list_long list);

void free_string_array(char** arr);

string_list array_to_string_list(char** arr);
