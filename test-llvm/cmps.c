void nop() {}

int main() {
    // test constant folding
    short _true = 32 == 32;
    short _false = 99 == 7;

    int a = 1;
    int b = 2;

    int c = a == b;
    if (a == b)
        nop();
    if (c)
        nop();

    unsigned int d = 3;
    unsigned int e = 4;
    unsigned int f = d == e;
    if (d != e)
        nop();
    if (f)
        nop();

	return 0;
}
