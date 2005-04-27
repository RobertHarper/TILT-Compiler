#include "s.h"
#include "r.h"

string
sysval_name(cerr er, char* errmsg, sysvals vs, int key)
{
	int n = vs->size;
	sysval v = vs->vec;
	for(; n>0; n--, v++)
		if(key==v->value) {
			string name = cstring2mlstring_alloc(v->name);
			return name;
		}
	send_errmsg(er,errmsg);
	return cstring2mlstring_alloc("");
}

int
sysval_num(cerr er, char* errmsg, sysvals vs, string mlkey)
{
	char* key = mlstring2cstring_static(mlkey);
	int n = vs->size;
	sysval v = vs->vec;
	for(; n>0; n--, v++)
		if(strcmp(key,v->name)==0)
			return v->value;
	send_errmsg(er,errmsg);
	return 0;
}
