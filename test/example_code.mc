matrix board;
struct player {points: int, num_pieces: int, location: duple};
player p;
vector vector_1;
vector vector_2;
duple loc;
int x;


board = matrix_create([[0, 1, 0],[1, 0, 1],[0, 1, 0]]);
loc = (1, 1);
p = player{points: 0, num_pieces: 4, location: loc};
x = 1;
vector_1 = horizontal(-1);
vector_2 = diagonalRight(x);


if (p.points == 0){
    vector_1 = vector_1 + vector_2;
} else{
    vector_1 = vector_1 - vector_2;
}

x = board[p.location];
board[p.location] = 0;
pi.location = p.location move vector_1;
board[p.location] = x;







