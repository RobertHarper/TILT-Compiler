#include <string.h>
#include "general.h"
#include "tag.h"
#include "thread.h"
#include "global.h"
#include "create.h"
#include "gc.h"
#include "forward.h"
#include "bitmap.h"

static unsigned char evilString[] = {
	51,0,6,0,52,0,5,0,75,0,4,0,76,0,3,0,
	84,0,2,0,85,0,1,0,86,0,35,2,0,0,
	51,0,31,0,56,0,30,0,57,0,29,0,0,0,
	51,0,6,0,52,0,5,0,75,0,43,0,76,0,3,0,0,0,
	51,0,6,0,52,0,5,0,75,0,44,0,76,0,3,0,0,0,
	0,0,
	0,0,
	1,0,46,0,55,0,45,0,0,0,
	2,0,56,0,3,0,55,0,26,0,54,0,27,0,53,0,
	28,0,52,0,34,0,51,0,39,0,50,0,0,0,
	0,0,
	1,0,73,0,71,0,72,0,0,0,
	2,0,56,0,3,0,55,0,26,0,54,0,27,0,53,0,
	28,0,52,0,34,0,75,0,39,0,50,0,43,0,74,0,0,0,
	41,0,80,0,42,0,79,0,0,0,
	1,0,84,0,77,0,83,0,0,0,
	1,0,86,0,70,0,85,0,0,0,
	1,0,87,0,0,0,
	1,0,90,0,5,0,89,0,50,0,88,0,0,0,
	0,0,
	51,0,6,0,52,0,5,0,75,0,92,0,76,0,3,0,0,0,
	3,0,93,0,0,0,
	3,0,94,0,0,0,
	83,0,95,0,0,0,
	83,0,97,0,0,0,
	1,0,99,0,80,0,98,0,0,0,
	2,0,56,0,3,0,55,0,27,0,53,0,28,0,52,0,
	37,0,104,0,38,0,103,0,39,0,102,0,40,0,101,0,
	43,0,100,0,0,0,
	4,0,106,0,49,0,105,0,0,0,
	42,0,109,0,45,0,108,0,0,0,
	42,0,109,0,45,0,110,0,0,0,
	1,0,84,0,77,0,111,0,0,0,
	51,0,6,0,52,0,5,0,75,0,112,0,76,0,3,0,0,0,
	51,0,31,0,56,0,113,0,57,0,29,0,0,0,
	0,0,
	1,0,46,0,55,0,114,0,0,0,
	4,0,116,0,62,0,115,0,0,0,
	42,0,118,0,61,0,117,0,0,0,
	1,0,120,0,59,0,119,0,0,0,
	1,0,86,0,70,0,121,0,0,0,
	1,0,90,0,5,0,124,0,64,0,123,0,65,0,122,0,0,0,
	1,0,127,0,58,0,126,0,0,0,
	1,0,129,0,60,0,128,0,0,0,
	1,0,131,0,63,0,130,0,0,0,
	42,0,118,0,61,0,132,0,0,0,
	42,0,109,0,45,0,133,0,0,0,
	51,0,31,0,56,0,134,0,57,0,29,0,0,0,
	0,0,
	0,0,
	0,0,
	1,0,46,0,55,0,135,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	2,0,56,0,3,0,55,0,27,0,53,0,28,0,52,0,
	39,0,137,0,0,0,
	0,0,
	0,0,
	0,0,
	2,0,56,0,3,0,55,0,26,0,141,0,27,0,53,0,
	28,0,52,0,39,0,50,0,0,0,
	2,0,56,0,3,0,55,0,26,0,143,0,27,0,53,0,
	28,0,52,0,32,0,142,0,39,0,50,0,0,0,
	2,0,56,0,3,0,55,0,26,0,145,0,27,0,53,0,
	28,0,52,0,39,0,50,0,0,0,
	2,0,56,0,3,0,55,0,26,0,143,0,27,0,53,0,
	28,0,52,0,32,0,147,0,39,0,50,0,0,0,
	2,0,152,0,6,0,151,0,29,0,150,0,30,0,149,0,0,0,
	0,0,
	0,0,
	1,0,157,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	79,0,161,0,0,0,
	2,0,56,0,3,0,55,0,26,0,54,0,27,0,53,0,
	28,0,52,0,34,0,163,0,39,0,50,0,0,0,
	0,0,
	2,0,56,0,3,0,55,0,26,0,145,0,27,0,53,0,
	28,0,52,0,39,0,50,0,44,0,164,0,0,0,
	2,0,168,0,36,0,167,0,43,0,166,0,0,0,
	0,0,
	0,0,
	0,0,
	44,0,174,0,0,0,
	0,0,
	0,0,
	68,0,176,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,90,0,5,0,89,0,50,0,182,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	83,0,185,0,0,0,
	0,0,
	0,0,
	69,0,188,0,79,0,187,0,0,0,
	2,0,56,0,3,0,55,0,27,0,53,0,28,0,52,0,
	37,0,104,0,38,0,191,0,39,0,102,0,40,0,101,0,0,0,
	0,0,
	35,0,193,0,0,0,
	0,0,
	0,0,
	0,0,
	1,0,197,0,0,0,
	0,0,
	0,0,
	1,0,200,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,204,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,90,0,5,0,124,0,65,0,212,0,0,0,
	0,0,
	1,0,127,0,58,0,213,0,0,0,
	0,0,
	72,0,216,0,79,0,215,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	2,0,56,0,3,0,55,0,26,0,54,0,27,0,53,0,
	28,0,52,0,34,0,221,0,39,0,50,0,0,0,
	0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,222,0,0,0,
	1,0,235,0,3,0,234,0,19,0,233,0,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	2,0,56,0,3,0,55,0,26,0,2,1,27,0,53,0,
	28,0,52,0,39,0,50,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,90,0,5,0,14,1,0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,15,1,0,0,
	1,0,73,0,71,0,16,1,0,0,
	0,0,
	51,0,31,0,56,0,19,1,57,0,29,0,78,0,18,1,0,0,
	0,0,
	0,0,
	0,0,
	2,0,168,0,36,0,23,1,0,0,
	0,0,
	35,0,25,1,0,0,
	44,0,164,0,0,0,
	2,0,26,1,0,0,
	0,0,
	0,0,
	41,0,28,1,42,0,79,0,0,0,
	0,0,
	1,0,84,0,77,0,30,1,0,0,
	0,0,
	1,0,33,1,67,0,32,1,0,0,
	1,0,33,1,67,0,35,1,0,0,
	1,0,86,0,70,0,36,1,0,0,
	1,0,33,1,67,0,37,1,0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,38,1,0,0,
	0,0,
	1,0,90,0,5,0,39,1,0,0,
	51,0,6,0,52,0,5,0,75,0,40,1,76,0,3,0,0,0,
	0,0,
	1,0,99,0,80,0,41,1,0,0,
	68,0,42,1,0,0,
	0,0,
	1,0,44,1,0,0,
	1,0,45,1,0,0,
	0,0,
	2,0,56,0,3,0,55,0,27,0,53,0,28,0,52,0,
	37,0,46,1,39,0,102,0,40,0,101,0,0,0,
	0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,48,1,0,0,
	2,0,56,0,3,0,55,0,27,0,53,0,28,0,52,0,
	37,0,104,0,38,0,49,1,39,0,102,0,40,0,101,0,0,0,
	4,0,106,0,49,0,50,1,0,0,
	0,0,
	41,0,53,1,42,0,79,0,0,0,
	42,0,109,0,45,0,54,1,0,0,
	0,0,
	41,0,56,1,42,0,79,0,0,0,
	51,0,6,0,52,0,58,1,54,0,57,1,0,0,
	4,0,116,0,62,0,61,1,0,0,
	0,0,
	42,0,118,0,61,0,63,1,0,0,
	0,0,
	1,0,120,0,59,0,65,1,0,0,
	1,0,33,1,67,0,66,1,0,0,
	1,0,90,0,5,0,67,1,0,0,
	1,0,90,0,5,0,124,0,64,0,68,1,65,0,122,0,0,0,
	1,0,90,0,5,0,70,1,65,0,69,1,0,0,
	0,0,
	0,0,
	1,0,129,0,60,0,71,1,0,0,
	0,0,
	0,0,
	1,0,73,1,0,0,
	1,0,131,0,63,0,74,1,0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,75,1,0,0,
	41,0,76,1,42,0,79,0,0,0,
	0,0,
	0,0,
	0,0,
	7,0,78,1,0,0,
	0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,81,1,
	13,0,80,1,0,0,
	2,0,152,0,6,0,84,1,8,0,83,1,9,0,82,1,0,0,
	0,0,
	0,0,
	0,0,
	1,0,235,0,3,0,234,0,20,0,87,1,21,0,231,0,
	24,0,230,0,0,0,
	0,0,
	0,0,
	0,0,
	1,0,235,0,3,0,234,0,20,0,92,1,21,0,231,0,
	24,0,230,0,0,0,
	51,0,6,0,52,0,58,1,54,0,93,1,0,0,
	0,0,
	1,0,235,0,3,0,234,0,19,0,95,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	0,0,
	25,0,96,1,0,0,
	1,0,235,0,3,0,234,0,19,0,100,1,20,0,232,0,
	21,0,231,0,22,0,99,1,24,0,230,0,0,0,
	1,0,235,0,3,0,234,0,18,0,104,1,19,0,103,1,
	20,0,232,0,21,0,231,0,23,0,102,1,24,0,230,0,0,0,
	1,0,235,0,3,0,234,0,19,0,100,1,20,0,232,0,
	21,0,231,0,22,0,106,1,24,0,230,0,0,0,
	2,0,152,0,6,0,110,1,16,0,109,1,17,0,108,1,0,0,
	1,0,235,0,3,0,234,0,19,0,112,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	1,0,235,0,3,0,234,0,19,0,113,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	1,0,114,1,0,0,
	51,0,6,0,52,0,58,1,54,0,115,1,0,0,
	1,0,235,0,3,0,234,0,19,0,116,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	2,0,152,0,6,0,117,1,0,0,
	2,0,56,0,3,0,55,0,14,0,120,1,15,0,119,1,
	26,0,118,1,27,0,53,0,28,0,52,0,39,0,50,0,0,0,
	1,0,235,0,3,0,234,0,19,0,121,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	2,0,56,0,3,0,55,0,26,0,143,0,27,0,53,0,
	28,0,52,0,32,0,123,1,39,0,50,0,0,0,
	0,0,
	2,0,56,0,3,0,55,0,26,0,143,0,27,0,53,0,
	28,0,52,0,32,0,124,1,39,0,50,0,0,0,
	2,0,56,0,3,0,55,0,26,0,126,1,27,0,53,0,
	28,0,52,0,33,0,125,1,39,0,50,0,0,0,
	0,0,
	0,0,
	2,0,152,0,6,0,151,0,29,0,150,0,30,0,127,1,0,0,
	2,0,56,0,3,0,55,0,26,0,128,1,27,0,53,0,
	28,0,52,0,39,0,50,0,0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,129,1,0,0,
	2,0,56,0,3,0,55,0,26,0,130,1,27,0,53,0,
	28,0,52,0,39,0,50,0,0,0,
	0,0,
	0,0,
	0,0,
	1,0,33,1,67,0,131,1,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	44,0,134,1,0,0,
	0,0,
	2,0,168,0,36,0,135,1,0,0,
	0,0,
	35,0,137,1,0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,138,1,0,0,
	0,0,
	0,0,
	0,0,
	1,0,90,0,5,0,140,1,73,0,139,1,0,0,
	0,0,
	0,0,
	51,0,31,0,56,0,144,1,57,0,29,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,90,0,5,0,149,1,81,0,148,1,0,0,
	0,0,
	0,0,
	0,0,
	1,0,235,0,3,0,234,0,19,0,151,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	0,0,
	0,0,
	0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,152,1,0,0,
	1,0,90,0,5,0,153,1,0,0,
	0,0,
	0,0,
	4,0,157,1,46,0,156,1,47,0,155,1,48,0,154,1,0,0,
	0,0,
	0,0,
	51,0,6,0,52,0,58,1,54,0,161,1,0,0,
	51,0,6,0,52,0,58,1,54,0,162,1,0,0,
	51,0,6,0,52,0,58,1,54,0,163,1,0,0,
	0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,164,1,0,0,
	0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,165,1,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,33,1,67,0,167,1,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,168,1,0,0,
	0,0,
	7,0,225,0,10,0,170,1,11,0,169,1,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	7,0,177,1,0,0,
	0,0,
	1,0,235,0,3,0,234,0,19,0,178,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	1,0,235,0,3,0,234,0,19,0,179,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,180,1,0,0,
	2,0,56,0,3,0,55,0,14,0,181,1,15,0,119,1,
	26,0,118,1,27,0,53,0,28,0,52,0,39,0,50,0,0,0,
	0,0,
	0,0,
	1,0,235,0,3,0,234,0,19,0,100,1,20,0,232,0,
	21,0,231,0,22,0,183,1,24,0,230,0,0,0,
	0,0,
	0,0,
	3,0,234,0,21,0,185,1,24,0,230,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,90,0,5,0,203,1,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	79,0,208,1,0,0,
	1,0,33,1,67,0,209,1,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	74,0,212,1,0,0,
	51,0,6,0,52,0,5,0,75,0,214,1,76,0,3,0,0,0,
	51,0,6,0,52,0,5,0,75,0,215,1,76,0,3,0,0,0,
	1,0,90,0,5,0,217,1,66,0,216,1,0,0,
	0,0,
	1,0,235,0,3,0,234,0,19,0,221,1,20,0,232,0,
	21,0,231,0,24,0,230,0,53,0,220,1,0,0,
	0,0,
	1,0,90,0,5,0,140,1,73,0,222,1,0,0,
	0,0,
	74,0,223,1,0,0,
	51,0,6,0,52,0,5,0,75,0,224,1,76,0,3,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,226,1,0,0,
	7,0,227,1,0,0,
	51,0,6,0,52,0,58,1,54,0,228,1,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,90,0,5,0,230,1,0,0,
	0,0,
	0,0,
	0,0,
	7,0,78,1,0,0,
	7,0,231,1,0,0,
	0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,233,1,
	13,0,232,1,0,0,
	0,0,
	2,0,152,0,6,0,84,1,8,0,83,1,9,0,234,1,0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,235,1,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,235,0,3,0,234,0,18,0,237,1,19,0,236,1,
	20,0,232,0,21,0,231,0,24,0,230,0,0,0,
	0,0,
	0,0,
	25,0,239,1,0,0,
	0,0,
	0,0,
	1,0,235,0,3,0,234,0,19,0,100,1,20,0,232,0,
	21,0,231,0,22,0,240,1,24,0,230,0,0,0,
	0,0,
	1,0,235,0,3,0,234,0,19,0,242,1,20,0,232,0,
	21,0,231,0,23,0,241,1,24,0,230,0,0,0,
	1,0,235,0,3,0,234,0,18,0,243,1,19,0,236,1,
	20,0,232,0,21,0,231,0,24,0,230,0,0,0,
	0,0,
	0,0,
	0,0,
	2,0,152,0,6,0,110,1,16,0,109,1,17,0,244,1,0,0,
	1,0,235,0,3,0,234,0,19,0,245,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	1,0,235,0,3,0,234,0,19,0,246,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	1,0,235,0,3,0,234,0,18,0,247,1,19,0,236,1,
	20,0,232,0,21,0,231,0,24,0,230,0,0,0,
	1,0,235,0,3,0,234,0,19,0,248,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	1,0,235,0,3,0,234,0,19,0,249,1,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	2,0,56,0,3,0,55,0,14,0,250,1,15,0,119,1,
	26,0,118,1,27,0,53,0,28,0,52,0,39,0,50,0,0,0,
	2,0,56,0,3,0,55,0,14,0,251,1,15,0,119,1,
	26,0,118,1,27,0,53,0,28,0,52,0,39,0,50,0,0,0,
	0,0,
	0,0,
	0,0,
	2,0,56,0,3,0,55,0,26,0,126,1,27,0,53,0,
	28,0,52,0,33,0,252,1,39,0,50,0,0,0,
	2,0,56,0,3,0,55,0,26,0,253,1,27,0,53,0,
	28,0,52,0,39,0,50,0,0,0,
	0,0,
	0,0,
	2,0,56,0,3,0,55,0,14,0,254,1,15,0,119,1,
	26,0,118,1,27,0,53,0,28,0,52,0,39,0,50,0,0,0,
	0,0,
	0,0,
	1,0,90,0,5,0,140,1,51,0,6,0,52,0,5,0,
	73,0,1,2,75,0,0,2,76,0,3,0,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	42,0,6,2,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	4,0,157,1,47,0,9,2,48,0,154,1,0,0,
	0,0,
	0,0,
	0,0,
	51,0,6,0,52,0,58,1,54,0,12,2,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	2,0,56,0,3,0,55,0,14,0,16,2,15,0,119,1,
	26,0,118,1,27,0,53,0,28,0,52,0,39,0,50,0,0,0,
	0,0,
	0,0,
	0,0,
	1,0,90,0,5,0,140,1,73,0,19,2,0,0,
	1,0,90,0,5,0,217,1,66,0,20,2,0,0,
	1,0,90,0,5,0,21,2,0,0,
	1,0,90,0,5,0,22,2,0,0,
	1,0,235,0,3,0,234,0,19,0,221,1,20,0,232,0,
	21,0,231,0,24,0,230,0,53,0,23,2,0,0,
	1,0,90,0,5,0,149,1,81,0,24,2,0,0,
	0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,25,2,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	1,0,235,0,3,0,234,0,19,0,27,2,20,0,232,0,
	21,0,231,0,24,0,230,0,0,0,
	0,0,
	74,0,28,2,0,0,
	74,0,29,2,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	51,0,6,0,52,0,58,1,54,0,33,2,0,0,
	0,0,
	0,0,
	0,0,
	0,0,
	7,0,225,0,10,0,224,0,11,0,223,0,12,0,34,2,0,0,
	0,0,
	0,0,
	0,0,
	0,0
};

#define evilLen (sizeof(evilString))
#define evilTag (WORD_ARRAY_TYPE | (evilLen << ARRLEN_OFFSET))

static int checkString (const unsigned char* s, int len, int verbose) {
	int minLen = evilLen > len ? len : evilLen;
	int result = 1;
	int i;
	if (len != evilLen) {
		result = 0;
		if (verbose) {
			printf("string: 0x%x differs in length (%d, expected %d)\n", s, len, evilLen);
		}
	}
	for (i=0; i < minLen; i++) {
		if (s[i] != evilString[i]) {
			result = 0;
			if (verbose) {
				printf("string: 0x%x differs at offset %d\n", s, i);
			}
			break;
		}
	}
	return result;
}

static mem_t theString = NULL;
static int tagCorrect=0, bodyCorrect=0;

static int checkBody(int verbose) {
	return checkString((unsigned char*) theString, evilLen, verbose);
}

static int checkTag(int verbose) {
	int tag = theString[-1];
	int result = 1;
	if (tag != evilTag) {
		result = 0;
		if (verbose) {
			printf("string: 0x%x has bad tag 0x%x\n", theString, tag);
		}
	}
	return result;
}
		
static void noticedObject (mem_t obj) {
	printf("string: 0x%x replacing 0x%x as theString\n", obj, theString);
	theString = obj;
	tagCorrect = 1;
	bodyCorrect = 0;
}

static void noticedInit(void) {
	printf("string: 0x%x has been initialized\n", theString);
}

static void noticedInitTag(void) {
	printf("string: 0x%x tag is now correct\n", theString);
}

static void noticedChange(void) {
	printf("string: 0x%x has changed\n", theString);
}

static void noticedChangedTag(void) {
	printf("string: 0x%x has a new tag of 0x%x\n", theString, theString[-1]);
}

/*
static void noticedEvilObject(mem_t obj) {
	printf("string: noticed an evil object at 0x%x\n", obj);
}
*/

INLINE(doCheck)
void doCheck(int* correct,
	     int (*check)(int),
	     void (*change)(void),
	     void (*init)(void)) {
	if (*correct) {
		if (! check(1)) {
			change();
			*correct = 0;
		}
	} else {
		if (check(0)) {
			init();
			*correct = 1;
		}
	}
}

void checkObj(mem_t obj) {
	if (obj != NULL && obj[-1] == evilTag && obj != theString) {
		noticedObject(obj);
	}
	if (theString != NULL) {
		/*
		if (theString[511] == 0x8002) {
			noticedEvilObject(theString + 512);
		}
		*/
		doCheck(&tagCorrect, checkTag, noticedChangedTag, noticedInitTag);
		doCheck(&bodyCorrect, checkBody, noticedChange, noticedInit);
	}
}

static void noticedBadBit(Bitmap_t* bitmap, int bit) {
	printf("string: 0x%x bad bit %d in bitmap 0x%x\n", theString, bit, bitmap);
}

void checkBitmap(Bitmap_t* bitmap, int largebitmapsize, mem_t bottom) {
	if (theString != NULL && tagCorrect && bodyCorrect) {
		mem_t stringStart;
		int byteLen = objectLength(theString, &stringStart);
		int bytePos = (sizeof (unsigned int)) * (stringStart - bottom);
		int chunkPos = DivideDown(bytePos, largebitmapsize);
		int chunkLen = DivideUp(byteLen, largebitmapsize);
		int isset = IsSet(bitmap, chunkPos);
		int i;
		assert(stringStart == theString - 1);
		if (isset) {
			for (i=0; i < chunkLen; i++) {
				if (!IsSet(bitmap, chunkPos + i)) {
					break;
				}
			}
		} else {
			for (i=0; i < chunkLen; i++) {
				if (IsSet(bitmap, chunkPos + i)) {
					break;
				}
			}
		}
		if (i != chunkLen) {
			printf("bitmap 0x%x byteLen=%d bytePos=%d chunkPos=%d chunkLen=%d isset=%d i=%d\n",
			       bitmap, byteLen, bytePos, chunkPos, chunkLen, isset, i);
			noticedBadBit(bitmap, chunkPos + i);
		}
	}
}
