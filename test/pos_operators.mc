# Arithmetic
int x;
x = (1 + 2) * 4;
int y;
y = x % 3 -1;

vector vector_1;
vector_1 = horizontal(-1);
vector_1 = vector_1 * 3;
vector vector_2;
vector_2 = horizontal(2);
vector_2 = vector_2 / 2;
vector vector_3;
vector_3 = vector_1 + vector_2;
vector_3 = vector_1 - vector_2;

# Equivalance
bool b1;
bool b2;
bool b3;
bool b4;
b1 = (vector_1 == vector_2);
b2 = (1 > 2);
b3 = (3<=4);
b4 = ((1,2) != (2,1));

# Logical
bool b5;
b5 = b1 and b2;
b5 = b1 or b2;
b5 = not b1;

# Move Function
struct Piece {name: string, location: duple};

struct Piece pi;
duple loc;
vector vector_1;

loc = (1, 1);
pi = Piece {name: “king”, location: loc};

# test four directions
vector_1 = horizontal(1);
pi.location = pi.location move vector_1;
vector_1 = vertical(-1);
pi.location = pi.location move vector_1;
vector_1 = diagonalLeft(1);
pi.location = pi.location move vector_1;
vector_1 = diagonalRight(1);
pi.location = pi.location move vector_1;
