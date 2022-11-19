long global_int = 0;
long global_int2 = 1;

long simple() {
	return 1;
}

long func2(long x) {
	/*return 10+x+global_int;*/
	return x+1;
}

long call_live(long x) {
	//return (long)&global_int;// + func2(x+2);
	return global_int2+10+x;
	/*return 1;*/
}

