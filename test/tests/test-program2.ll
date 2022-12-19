; ModuleID = 'dabor'
source_filename = "dabor"

%Player = type { i32, i32, [2 x i32] }

@board = global [5 x [5 x i32]] zeroinitializer
@p = global %Player zeroinitializer
@start = global i32* null
@dir = global i32* null
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.2 = private unnamed_addr constant [10 x i8] c"(%d, %d)\0A\00", align 1
@fmt.3 = private unnamed_addr constant [10 x i8] c"<%d, %d>\0A\00", align 1
@0 = global i32* null
@1 = global i32* null

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %0 = bitcast i8* %malloccall to i32*
  %1 = getelementptr i32, i32* %0, i32 0
  store i32 0, i32* %1, align 4
  %2 = getelementptr i32, i32* %0, i32 1
  store i32 0, i32* %2, align 4
  store i32* %0, i32** @start, align 8
  store i32 0, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 0), align 4
  store i32 5, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 1), align 4
  %start = load i32*, i32** @start, align 8
  store i32* %start, i32** bitcast ([2 x i32]* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 2) to i32**), align 8
  store [5 x [5 x i32]] [[5 x i32] [i32 1, i32 0, i32 0, i32 0, i32 0], [5 x i32] [i32 0, i32 2, i32 0, i32 0, i32 0], [5 x i32] [i32 0, i32 0, i32 3, i32 0, i32 0], [5 x i32] [i32 0, i32 0, i32 0, i32 4, i32 0], [5 x i32] [i32 0, i32 0, i32 0, i32 0, i32 5]], [5 x [5 x i32]]* @board, align 4
  %malloccall1 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %3 = bitcast i8* %malloccall1 to i32*
  %4 = getelementptr i32, i32* %3, i32 0
  store i32 1, i32* %4, align 4
  %5 = getelementptr i32, i32* %3, i32 1
  store i32 1, i32* %5, align 4
  store i32* %3, i32** @dir, align 8
  br label %while

while:                                            ; preds = %while_body, %entry
  %p10 = load i32, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 1), align 4
  %tmp11 = icmp sgt i32 %p10, 0
  br i1 %tmp11, label %while_body, label %merge

while_body:                                       ; preds = %while
  %p = load i32, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 0), align 4
  %p2 = load [2 x i32], [2 x i32]* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 2), align 4
  store [2 x i32] %p2, [2 x i32]* bitcast (i32** @0 to [2 x i32]*), align 4
  %6 = load i32*, i32** @0, align 8
  %7 = getelementptr inbounds i32, i32* %6, i32 0
  %i = load i32, i32* %7, align 4
  %8 = getelementptr inbounds i32, i32* %6, i32 1
  %j = load i32, i32* %8, align 4
  %board = getelementptr [5 x [5 x i32]], [5 x [5 x i32]]* @board, i32 0, i32 %i, i32 %j
  %board3 = load i32, i32* %board, align 4
  %tmp = add i32 %p, %board3
  store i32 %tmp, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 0), align 4
  %p4 = load [2 x i32], [2 x i32]* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 2), align 4
  store [2 x i32] %p4, [2 x i32]* bitcast (i32** @1 to [2 x i32]*), align 4
  %9 = load i32*, i32** @1, align 8
  %dir = load i32*, i32** @dir, align 8
  %10 = getelementptr inbounds i32, i32* %9, i32 0
  %11 = load i32, i32* %10, align 4
  %12 = getelementptr inbounds i32, i32* %9, i32 1
  %13 = load i32, i32* %12, align 4
  %14 = getelementptr inbounds i32, i32* %dir, i32 0
  %15 = load i32, i32* %14, align 4
  %16 = getelementptr inbounds i32, i32* %dir, i32 1
  %17 = load i32, i32* %16, align 4
  %tmp5 = add i32 %11, %15
  %tmp6 = add i32 %13, %17
  %malloccall7 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %18 = bitcast i8* %malloccall7 to i32*
  %19 = getelementptr i32, i32* %18, i32 0
  store i32 %tmp5, i32* %19, align 4
  %20 = getelementptr i32, i32* %18, i32 1
  store i32 %tmp6, i32* %20, align 4
  store i32* %18, i32** bitcast ([2 x i32]* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 2) to i32**), align 8
  %p8 = load i32, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 1), align 4
  %tmp9 = sub i32 %p8, 1
  store i32 %tmp9, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 1), align 4
  br label %while

merge:                                            ; preds = %while
  %p12 = load i32, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 0), align 4
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %p12)
  ret i32 0
}

declare noalias i8* @malloc(i32)
