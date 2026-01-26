// no need for headers in this test
int printf(const char *__restrict, ...);
char *strcat (char *__restrict, const char *__restrict);

typedef char *string;

string (string s) concat(string o) __attribute__((symbol("strcat")));
string (string s) __iadd__(string o) __attribute__((symbol("strcat")));

char str_hello[100] = "Hello";

typedef struct Point {
	float x;
	float y;
} Point;

inline Point* (Point* p) __iadd__(Point q) {
	p->x += q.x;
	p->y += q.y;
	// return q; // Error: the function shall always return the receiver 'p'
	return p;
}

Point (const Point p) __add__(const Point q) {
	Point r;
	r.x = p.x + q.x;
	r.y = p.y + q.y;
	return r;
}

int main(int argc, char const *argv[]) {
	str_hello.concat(", ");
	str_hello += "World!";
	printf("%s\n", str_hello);

	Point a = {1, 2};
	Point b = {3, 4};
	Point c = a + b;
	printf("c: %f %f\n", c.x, c.y);
	Point *d = (&a) += c;
	printf("d: %f %f\n", d->x, d->y);

	return 0;
}
