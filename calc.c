#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

typedef double (*binary_op)(double, double);
typedef double (*unary_op)(double);

typedef enum {
	BINARY_OP, UNARY_OP, JUMP, QUIT
} op_type_t;

typedef struct menu_item menu_item_t;
typedef struct {
	menu_item_t *menu;
	int size;
} menu_t;

typedef struct {
	op_type_t type;
	binary_op binop;
	unary_op unop;
	menu_t target;
} op_t;

typedef struct menu_item {
	const char *name;
	char key;
	op_t op;
} menu_item_t;

double add(double x, double y) {
	return x + y;
}

double sub(double x, double y) {
	return x - y;
}

double mul(double x, double y) {
	return x * y;
}

double div_(double x, double y) {
	return x / y;
}

double fact(double x) {
	if (x < 0) return 0;
	else {
		int retval = 1;
		int i = (int)x;
		while (i != 0) retval *= i--;
		return retval;
	}
}

double inverse(double x) {
	return 1/x;
}

double cube(double x) {
	return x*x*x;
}

double square(double x) {
	return x*x;
}

static menu_item_t main_menu[] = {
	{ "简易型", 'j', { JUMP, 0, 0, 0 } },
	{ "科学型", 'k', { JUMP, 0, 0, 0 } },
	{ "退出",   'x', { QUIT, 0, 0, 0 } }
};

static menu_item_t simple_menu[] = {
	{ "加",   'a', { BINARY_OP, add, 0, 0 } },
	{ "减",   's', { BINARY_OP, sub, 0, 0 } },
	{ "乘",   'm', { BINARY_OP, mul, 0, 0 } },
	{ "除",   'd', { BINARY_OP, div_, 0, 0 } },
	{ "向上", 'r', { JUMP, 0, 0, 0 } },
	{ "退出", 'x', { QUIT, 0, 0, 0 } }
};

static menu_item_t scientific_menu[] = {
	{ "正弦", 'i', { UNARY_OP,  0,   sin, 0 } },
	{ "余弦", 'c', { UNARY_OP,  0,   cos, 0 } },
	{ "正切", 't', { UNARY_OP,  0,   tan, 0 } },
	{ "x^y",  'm', { BINARY_OP, pow, 0, 0 } },
	{ "ln",   'e', { UNARY_OP,  0,   log, 0 } },
	{ "log",  's', { UNARY_OP,  0,   log10, 0 } },
	{ "n!",   'j', { UNARY_OP,  0,   fact, 0 } },
	{ "1/x",  'd', { UNARY_OP,  0,   inverse, 0 } },
	{ "立方", 'u', { UNARY_OP,  0,   cube, 0 } },
	{ "平方", 'p', { UNARY_OP,  0,   square, 0 } },
	{ "向上", 'r', { JUMP,      0,   0, 0 } },
	{ "退出", 'x', { QUIT,      0,   0, 0 } }
};

void link_menus() {
	int nmain = sizeof(main_menu) / sizeof(menu_item_t);
	int nsimple = sizeof(simple_menu) / sizeof(menu_item_t);
	int nscientific = sizeof(scientific_menu) / sizeof(menu_item_t);
	menu_t main_target = { main_menu, nmain };
	menu_t simple_target = { simple_menu, nsimple };
	menu_t scientific_target = { scientific_menu, nscientific };
	main_menu[0].op.target = simple_target;
	main_menu[1].op.target = scientific_target;
	simple_menu[nsimple - 2].op.target = main_target;
	scientific_menu[nscientific - 2].op.target = main_target;
};

void clear() {
	system("cls");
}

void display_menu(menu_t menu) {
	int i;
	clear();
	if (menu.menu == main_menu) puts("----------计算器----------");
	for (i = 0; i < menu.size; ++i) {
		printf("%s(%c)", menu.menu[i].name, menu.menu[i].key);
		if (i != menu.size - 1) putchar('/');
	}
}

int main() {
	int ch;
	int i;
	double lhs, rhs;
	menu_t current_menu = { main_menu, sizeof(main_menu) / sizeof(menu_item_t) };
	link_menus();
	for (;;) {
		display_menu(current_menu);
		while (isspace(ch = getchar()));
		if (ch == EOF) exit(EXIT_FAILURE);
		for (i = 0; i < current_menu.size; ++i) {
			if (current_menu.menu[i].key == ch) break;
		}
		if (i == current_menu.size) continue;
		switch (current_menu.menu[i].op.type) {
		case BINARY_OP:
			clear();
			scanf("%lf%lf", &lhs, &rhs);
			printf("%f", current_menu.menu[i].op.binop(lhs, rhs));
			while (isspace(getchar()));
			break;
		case UNARY_OP:
			clear();
			scanf("%lf", &lhs);
			printf("%f", current_menu.menu[i].op.unop(lhs));
			while (isspace(getchar()));
			break;
		case JUMP:
			current_menu = current_menu.menu[i].op.target;
			break;
		case QUIT:
			exit(EXIT_SUCCESS);
		}
	}
	return 0;
}
