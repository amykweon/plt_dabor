#primary data types
int i;
i = 32;

string s;
s = "hello";

bool b;
b = true;
b = false;

#matrix assignment
matrix board;
board = matrix_create([[0, 1, 0],[1, 0, 1],[0, 1, 0]]);
board = matrix_create([[“water”, “water”, “land”],[“water”, “land”, “land”],[“land”, “land”, “land”]]);
board = matrix_create([“water”, “water”, “water”],[1, 0, 1],[0, 1, 0]]);
board[1,2] = 1

#struct assignment
struct player {points: int, num_pieces: int};
struct player p;
p = player{points: 0, num_pieces: 4};
p.points = p.points + 1;

#vector assignment
vector vector_1;
vector_1 = horizontal(-1);
vector_1 = vertical(1);
vector_1 = diagonalLeft(1);
vector_1 = diagonalRight(-1);

#duple assignment
duple dir;
dir = (10, 8);

