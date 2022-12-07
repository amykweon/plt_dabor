//primary data types
int i;
string s;
bool b;
matrix board;
struct player {points: int, num_pieces: int};
struct player p;
vector vector_1;
duple dir;

i = 32;
s = "hello";
b = true;
b = false;

board = matrix_create([[0, 1, 0],[1, 0, 1],[0, 1, 0]]);
board[1,2] = 1;

p = player{points: 0, num_pieces: 4};
p.points = p.points + 1;

vector_1 = horizontal(1);
vector_1 = vertical(1);
vector_1 = diagonalLeft(1);
vector_1 = diagonalRight(1);

dir = (10, 8);


//