//Primary Data Types

//mismatch between type and value
int i;
i = "hello";

//mismatch between type and value
string s;
s = 1;

//wrong assignment of bool type
bool b;
b = True;

//Matrix Assignment

//not a valid matrix
matrix board1;
board1 =  matrix_create([[0, 1, 0], [1], [2, 1]]);

//access value out of matrix (haven't implemented this yet)
matrix board2;
board2 = matrix_create([[0, 1, 0],[1, 0, 1],[0, 1, 0]]);
board2[10,10] = 1;

//Struct Assignment
struct player {points: int, num_pieces: int};
struct player p;

//access undefined attribute in a struct
p = player{coordinate: 0, num_pieces: 1};

//attribute type mismatch in a struct
p = player{points: "hello", num_pieces: 1};

//Vector Assignment
//wrong type of param
vector vector_1;
vector_1 = horizontal("hello");

//Duple Assignment
duple dir;
//miss 1 param
dir = (10);
//wrong type of param
dir = ("hello","hello");
