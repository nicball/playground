#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ncurses.h>

struct point {
    int x;
    int y;
};

struct bone {
    struct bone * next;
    struct bone * prev;
    struct point pos;
};

enum direction {
    UP, DOWN, LEFT, RIGHT
};

struct snake {
    struct bone * head;
    struct bone * tail;
    enum direction heading;
    int length;
};

int point_equal(struct point a, struct point b) {
    return a.x == b.x && a.y == b.y;
}

struct bone * new_bone(struct bone * next, struct bone * prev, int x, int y) {
    struct bone * b = (struct bone *) malloc(sizeof(struct bone));
    b->next = next;
    b->prev = prev;
    b->pos.x = x;
    b->pos.y = y;
    return b;
}

void del_bone(struct bone * b) {
    free(b);
}

struct bone * grow_bone(struct bone * head, enum direction dir) {
    int x = head->pos.x;
    int y = head->pos.y;
    switch (dir) {
    case UP:
        head->next = new_bone(NULL, head, x, y - 1);
        return head->next;
    break;
    case DOWN:
        head->next = new_bone(NULL, head, x, y + 1);
        return head->next;
    break;
    case LEFT:
        head->next = new_bone(NULL, head, x - 1, y);
        return head->next;
    break;
    case RIGHT:
        head->next = new_bone(NULL, head, x + 1, y);
        return head->next;
    break;
    }
}

enum direction reverse_dir(enum direction d) {
    switch (d) {
    case UP: return DOWN;
    case DOWN: return UP;
    case LEFT: return RIGHT;
    case RIGHT: return LEFT;
    }
}

static const int DEFAULT_SNAKE_LENGTH = 5;
static const enum direction DEFAULT_SNAKE_HEADING = UP;

struct snake * new_snake_hl(int x, int y, enum direction heading, int length) {
    struct bone * tail = new_bone(NULL, NULL, x, y);
    struct bone * head = tail;
    for (int i = 0; i < length - 1; i++) {
        head = grow_bone(head, heading);
    }
    struct snake * s = (struct snake *) malloc(sizeof(struct snake));
    s->head = head;
    s->tail = tail;
    s->heading = heading;
    s->length = length;
    return s;
}

struct snake * new_snake(int x, int y) {
    return new_snake_hl(x, y, DEFAULT_SNAKE_HEADING, DEFAULT_SNAKE_LENGTH);
}

void del_snake(struct snake * s) {
    struct bone * i = s->tail;
    while (i != NULL) {
        struct bone * next = i->next;
        del_bone(i);
        i = next;
    }
    free(s);
}

void step_snake(struct snake * s) {
    struct bone * old_tail = s->tail;
    s->tail = s->tail->next;
    s->tail->prev = NULL;
    del_bone(old_tail);
    s->head = grow_bone(s->head, s->heading);
}

void grow_snake(struct snake * s, struct point new_tail_pos) {
    s->tail = new_bone(s->tail, NULL, new_tail_pos.x, new_tail_pos.y);
    s->tail->next->prev = s->tail;
    s->length++;
}

void check_clock_support(void) {
    if (clock() == (clock_t) -1) {
        puts("ERROR clock() is unavailable on your platform.");
        abort();
    }
}

void init_rand(void) {
    srand(time(NULL));
}

void with_ncurses(void (*action)(int, int)) {
    int nrow, ncol;
    initscr();
    getmaxyx(stdscr, nrow, ncol);
    cbreak();
    keypad(stdscr, TRUE);
    noecho();
    action(nrow, ncol);
    timeout(-1);
    attrset(A_NORMAL);
    mvprintw(nrow-2, 1, "Press any key...");
    getch();
    endwin();
}

void draw_border(void) {
    box(stdscr, 0, 0);
}

void draw_bone(struct bone * b) {
    if (b->next == NULL) {
        mvaddch(b->pos.y, b->pos.x, 'O');
    }
    else if (b->prev == NULL) {
        mvaddch(b->pos.y, b->pos.x, 'o');
    }
    else if (b->next->pos.x == b->prev->pos.x) {
        mvaddch(b->pos.y, b->pos.x, 'k');
    }
    else if (b->next->pos.y == b->prev->pos.y) {
        mvaddch(b->pos.y, b->pos.x, '=');
    }
    else {
        mvaddch(b->pos.y, b->pos.x, '+');
    }
}

void draw_snake(struct snake * s) {
    for (struct bone * b = s->tail;
         b != NULL;
         b = b->next) {
        draw_bone(b);
    }
}

void draw_food(struct point p) {
    mvaddch(p.y, p.x, '*');
}

void redraw(struct snake * s, struct point f) {
    clear();
    draw_border();
    draw_food(f);
    draw_snake(s);
}

void msg_center(int nrow, int ncol, const char * str) {
    attron(A_STANDOUT);
    mvaddstr(nrow/2, (ncol-strlen(str))/2, str);
    attroff(A_STANDOUT);
}

void display_lose(int nrow, int ncol) {
    clear();
    draw_border();
    msg_center(nrow, ncol, "YOU LOSE.");
}

void display_win(int nrow, int ncol) {
    clear();
    draw_border();
    msg_center(nrow, ncol, "YOU WIN!");
}

struct point generate_food(int nrow, int ncol, struct snake * s) {
    struct point p;
    p.x = rand() % (ncol-2) + 1;
    p.y = rand() % (nrow-2) + 1;
    for (struct bone * b = s->tail;
         b != NULL;
         b = b->next) {
        if (point_equal(b->pos, p)) {
            return generate_food(nrow, ncol, s);
        }
    }
    return p;
}

int out_of_border(struct point p, int nrow, int ncol) {
    return p.x <= 0
        || p.x >= ncol-1
        || p.y <= 0
        || p.y >= nrow-1;
}

int eat_self(struct snake * s) {
    struct point head_pos = s->head->pos;
    for (struct bone * b = s->tail;
         b != s->head;
         b = b->next) {
        if (point_equal(b->pos, head_pos)) {
            return TRUE;
        }
    }
    return FALSE;
}

enum direction to_dir(int ch) {
    switch (ch) {
    case KEY_UP: return UP;
    case KEY_DOWN: return DOWN;
    case KEY_LEFT: return LEFT;
    case KEY_RIGHT: return RIGHT;
    default: abort();
    }
}

static const int SAMPLING_RATE = 100;
static double GAME_SPEED = 1;

void init_speed(void) {
    puts("Please enter game speed (default to 1):");
    char line[80] ;
    fgets(line, 80, stdin);
    if (sscanf(line, "%lf", &GAME_SPEED) != 1) {
        GAME_SPEED = 1;
    }
}

void gameloop(int nrow, int ncol) {
    timeout(1000 / SAMPLING_RATE);
    const clock_t checkpoint = (clock_t) (CLOCKS_PER_SEC/GAME_SPEED);
    clock_t last_update = clock();
    struct snake * snake = new_snake(ncol/2, nrow/2);
    struct point food_pos = generate_food(nrow, ncol, snake);
    redraw(snake, food_pos);
    for (;;) {
        struct point tail_pos = snake->tail->pos;
        int ch;
        if ((ch = getch()) != ERR) {
            switch (ch) {
            case KEY_UP:
            case KEY_DOWN:
            case KEY_LEFT:
            case KEY_RIGHT:
                if (to_dir(ch) == reverse_dir(snake->heading)) {
                    break;
                }
                else {
                    snake->heading = to_dir(ch);
                    step_snake(snake);
                    tail_pos = snake->tail->pos;
                    redraw(snake, food_pos);
                }
                break;
            default: break;
            }
        }
        if (clock() - last_update >= checkpoint) {
            step_snake(snake);
            tail_pos = snake->tail->pos;
            redraw(snake, food_pos);
            last_update = clock();
        }
        if (point_equal(snake->head->pos, food_pos)) {
            grow_snake(snake, tail_pos);
            food_pos = generate_food(nrow, ncol, snake);
            redraw(snake, food_pos);
        }
        if (out_of_border(snake->head->pos, nrow, ncol)
         || eat_self(snake)) {
            display_lose(nrow, ncol);
            return;
        }
        if (snake->length == (nrow-2)*(ncol-2)) {
            display_win(nrow, ncol);
            return;
        }
    }
}

int main() {
    check_clock_support();
    init_rand();
    init_speed();
    with_ncurses(gameloop);
    return 0;
}
