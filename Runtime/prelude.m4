divert(-1)

dnl	The following bit protects our program from name clashes with
dnl	predefined m4 macros.

define(`m4define',defn(`define'))		undefine(`define')
m4define(`m4undefine',defn(`undefine'))		m4undefine(`undefine')
m4define(`m4defn',defn(`defn'))			m4undefine(`defn')
m4define(`m4ifelse',m4defn(`ifelse'))		m4undefine(`ifelse')
m4define(`m4shift',m4defn(`shift'))		m4undefine(`shift')

m4define(`hide',
	 `m4define(`m4$1',m4defn(`$1'))m4undefine(`$1')')

m4define(`hideall',
	 `m4ifelse(`$1',`', , `hide(`$1')hideall(m4shift($@))')')
	 
hideall(`len',`decr',`dlen',`eval',`incr',
	`popdef',`traceoff',`unix',`maketemp',
	`divert',`sinclude',`syscmd',`divnum',`sysval',`substr',
	`undivert',`translit',`errprint',`ifdef',`dnl',`changecom',
	`index',`include',`dumpdef',`traceon',`pushdef',
	`changequote')

m4undefine(`hide')
m4undefine(`hideall')


dnl	INLINE(functionName)
	
m4ifdef(`alpha_osf',
	`m4define(`INLINE',
		  `#pragma inline $1
static')',
	`m4define(`INLINE', `static inline')')

dnl	DISCARD_VOLATILE(type, exp)
m4define(`DISCARD_VOLATILE', `(($1)($2))')

m4divert`'m4dnl
