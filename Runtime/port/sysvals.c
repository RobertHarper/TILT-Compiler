#include "s.h"
#include "r.h"

/*string*/cresult
sysval_name(char* what, sysvals vs, int key)
{
	int n = vs->size;
	sysval v = vs->vec;
	for(; n>0; n--, v++)
		if(key==v->value) {
			string name = cstring2mlstring_alloc(v->name);
			return NormalPtr(name);
		}
	return Error(SysErr_fmt("%s: bad value: %d", what, key));
}

/*int*/cresult
sysval_num(char* what, sysvals vs, string mlkey)
{
	char* key = mlstring2cstring_static(mlkey);
	int n = vs->size;
	sysval v = vs->vec;
	for(; n>0; n--, v++)
		if(strcmp(key,v->name)==0)
			return Normal((val_t) v->value);
	return Error(SysErr_fmt("%s: bad name: %s",what,key));
}
