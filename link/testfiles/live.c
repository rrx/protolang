long func2(long x) {
	return 10+x;
}

long call_live(long x) {
	return func2(x+2);
}

