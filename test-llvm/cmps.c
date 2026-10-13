void nop() {}

int main() {
    // test constant folding
    short _true = 32 == 32;
    short _false = 99 == 7;

    int a = 1;
    int b = 2;

    int c  = a == b;
    int c2 = a != b;
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

    _true  = b > a;
    _false = b < a;
    _true  = e >= d;
    _false = e <= d;

    // Test ult vs slt
    _true  = a < e;
    _false = e < a;

    // Test LOGOR and LOGAND
    int g = _true || _false;
    int h = g && _false;
    int i = h && _false || 0;
    int j = i || _false && 1;
    int k = j && (_false || _true);
    int l = (_true && _false) || _false;
    if (l)
        nop();

    if (_true || _false)
        nop();

    if (_true && _false)
        nop();

    if (_true && _false || 0)
        nop();

    if (_true || _false && 1)
        nop();

    if (_true && (!_true || _false))
        nop();

    if ((_true && !_false) || _false)
        nop();


    int x = 3;
    while (x) {
        x--;
        nop();
    }

    for (int x = 0; x < 10; x++) {
        nop();
    }

    do {
        nop();
        x++;
    } while (x < 10);

    switch (x) {
        case 0 ... 1:
            nop();
            break;
        case 2 ... 3:
            nop();
            nop();
            nop();
            break;
    }

    switch (x) {
    case 1:
        nop();
    case 2:
        nop();
        nop();
    case 3 ... 5:
        nop();
        nop();
        nop();
        break;
    default:
        nop();
        nop();
        nop();
        nop();
        break;
    }

	return 0;
}
