# Arithmetic
# vectors of different directions cannot be added together
vector vector_1;
vector vector_2;
vector vector_3;
vector_1 = horizontal(-1);
vector_2 = vertical(1);
vector_3 = vector_1 + vector_2;

# Logical

# Equivalance
# param1 and param2 must have the same type
bool b1;
b1 = (vector_1 > 2);

#Move