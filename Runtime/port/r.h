/*
	val_t
		An ML value that may or may not be a pointer.
	ptr_t
		An ML value that has a pointer type.  It might be a
		small-valued constructor (<= 256), a pointer into global data,
		a pointer into stack data, or a pointer into one of the heaps.
	uct
		Ugly C Thing.  C pointers cast to word and stashed
		away by ML.
*/
typedef uint32	val_t;
typedef uint32*	ptr_t;
typedef uint32	uct;

typedef ptr_t	unit;
#	define empty_record	((unit)256)

typedef int32	bool;
enum { false, true };

typedef uint32	word;

typedef uint8	word8;

typedef ptr_t	record;
typedef ptr_t	string;
typedef string	word8array;
typedef const string	word8vector;
typedef ptr_t	list;
typedef ptr_t	option;
typedef ptr_t	exn;

typedef struct cerr* cerr;

#define PAIR(t,t1,t2)\
	struct t {\
		t1	first;\
		t2	second;\
	};\
	typedef struct t* t##_long;\
	typedef ptr_t t;

#define TRIPLE(t,t1,t2,t3)\
	struct t {\
		t1	first;\
		t2	second;\
		t3	third;\
	};\
	typedef struct t* t##_long;\
	typedef ptr_t t;

#define LIST(t,tlist)\
	struct tlist {\
		t	car;\
		ptr_t	cdr;\
	};\
	typedef struct tlist* tlist##_long;\
	typedef ptr_t tlist;

#define OPTION(t,topt)\
	typedef t topt;

#define ARRAY_OPTION(t)\
	typedef ptr_t t;

PAIR(intpair,int32,int32)
OPTION(intpair,intpair_option)
ARRAY_OPTION(string_option)
TRIPLE(inttriple,int32,int32,int32)
PAIR(wordpair,word,word)
PAIR(intword,int32,word)
LIST(intword,intword_list)
LIST(word,word_list)
LIST(string,string_list)
PAIR(string_stringlist,string,string_list)
TRIPLE(string_word_stringlist,string,word,string_list)
PAIR(stringpair,string,string)

#undef PAIR
#undef TRIPLE
#undef LIST
#undef OPTION
#undef ARRAY_OPTION

typedef struct sysval* sysval;
typedef struct sysvals* sysvals;

struct sysval {
	char* name;
	int value;
};

struct sysvals {
	int size;
	sysval vec;
};

enum {
	/*
		Must agree with ../../Basis/Numeric/tiltfc.sml.  These
		values are just integers known to the basis and the
		runtime; in particular, they are not tags.
	*/
	TO_NEAREST=0, TO_ZERO, TO_POSINF, TO_NEGINF,
	SINGLE=0, DOUBLE, EXTENDED,
};

#define arraysize(a)	(sizeof(a)/sizeof(*(a)))
#define USED(x)	if(x){}else{}

/* sysvals.c */
string	sysval_name(cerr, char* errmsg, sysvals, int key);
int	sysval_num(cerr, char* errmsg, sysvals, string key);

/* die.c */
void	DIE(char* msg);
void	DIEwith(int e);

/* ../sparc/malloc.c ../talx86/malloc.c */
void*	emalloc(size_t size);
void*	emalloc_atomic(size_t size);
void*	erealloc(void* buf, size_t size);
void	efree(void*);
#define	enew(t)	((t*)emalloc(sizeof(t)))
#define	enew_atomic(t)	((t*)emalloc_atomic(sizeof(t)))

/* portmalloc.c */
char*	estrdup(char*);

/* ../sparc/create.c ../talx86/create.c */
val_t	get_record(ptr_t rec, int which);
ptr_t	alloc_record(val_t* fields, int count);
string	alloc_uninit_string(int strlen, char** raw);
void	adjust_stringlen(string, int);
int	stringlen(string);
char*	stringbuf(string);
/*
	The sparc and talx86 backends use different representations for ML
	array options (including string options).
*/
ptr_t	valof_arrayopt(option);
option	some_array(ptr_t);
char*	mlstring2cstring_static(string);

/* portcreate.c */
record	alloc_intint(val_t, val_t);
string	alloc_string(int bufsize, char* buf);
string	cstring2mlstring_alloc(char*);
char*	mlstring2cstring_buffer(string, int bufsize, char* buf);
char*	mlstring2cstring_malloc(string);	/* allocate a new buffer for each call */
/*
	A C string option is either NULL or a non-null pointer to a
	NUL-terminated string.  The C type is char*.
*/
string_option	alloc_string_option(char* cstringopt);
/*
	Translate an ML string list of length n to an n+1 element array of
	NUL-terminated strings where the last element is NULL.  The
	returned array v and all of the string data may be freed en masse
	with efree(v).
*/
char**	string_list_to_array_malloc(string_list);

/* portexn.c */
val_t	getSubStamp(void);
val_t	getDivStamp(void);
val_t	getOvflStamp(void);
string	exnNameRuntime(exn);
string	exnMessageRuntime(exn);

/* ccall.c */
void	send_errno(cerr, int);
void	send_errmsg(cerr,char*);
