#define SWAP(a, b, type) { type c; c = b; b = a; a = c; }

#include &lt;stdio.h&gt;
int main()
{
	int q = 4;
	int y = 5;

	SWAP(q, y, int);

	printf("q is %d\ny is %d\n", q, y);

	return 0;
}
