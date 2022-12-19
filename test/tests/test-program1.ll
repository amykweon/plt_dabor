; ModuleID = 'dabor'
source_filename = "dabor"

%Player = type { i32, i32, [2 x i32] }

@board = global [3 x [3 x i32]] zeroinitializer
@p = global %Player zeroinitializer
@vector_1 = global i32* null
@vector_2 = global i32* null
@loc = global i32* null
@x = global i32 0
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.2 = private unnamed_addr constant [10 x i8] c"(%d, %d)\0A\00", align 1
@fmt.3 = private unnamed_addr constant [10 x i8] c"<%d, %d>\0A\00", align 1
@fmt.4 = private unnamed_addr constant [31 x i8] c"%d %d %d \0A%d %d %d \0A%d %d %d \0A\00", align 1
@0 = global i32* null
@1 = global i32* null
@2 = global i32* null
@3 = global i32* null
@fmt.5 = private unnamed_addr constant [31 x i8] c"%d %d %d \0A%d %d %d \0A%d %d %d \0A\00", align 1

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  store [3 x [3 x i32]] [[3 x i32] [i32 0, i32 1, i32 0], [3 x i32] [i32 1, i32 0, i32 1], [3 x i32] [i32 0, i32 1, i32 0]], [3 x [3 x i32]]* @board, align 4
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %0 = bitcast i8* %malloccall to i32*
  %1 = getelementptr i32, i32* %0, i32 0
  store i32 1, i32* %1, align 4
  %2 = getelementptr i32, i32* %0, i32 1
  store i32 0, i32* %2, align 4
  store i32* %0, i32** @loc, align 8
  store i32 0, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 0), align 4
  store i32 4, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 1), align 4
  %loc = load i32*, i32** @loc, align 8
  store i32* %loc, i32** bitcast ([2 x i32]* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 2) to i32**), align 8
  store i32 1, i32* @x, align 4
  %malloccall1 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %3 = bitcast i8* %malloccall1 to i32*
  %4 = getelementptr i32, i32* %3, i32 0
  store i32 0, i32* %4, align 4
  %5 = getelementptr i32, i32* %3, i32 1
  store i32 -1, i32* %5, align 4
  store i32* %3, i32** @vector_1, align 8
  %x = load i32, i32* @x, align 4
  %x2 = load i32, i32* @x, align 4
  %malloccall3 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %6 = bitcast i8* %malloccall3 to i32*
  %7 = getelementptr i32, i32* %6, i32 0
  store i32 %x, i32* %7, align 4
  %8 = getelementptr i32, i32* %6, i32 1
  store i32 %x2, i32* %8, align 4
  store i32* %6, i32** @vector_2, align 8
  %p = load i32, i32* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 0), align 4
  %tmp = icmp eq i32 %p, 0
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else, %then
  %9 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 0, i32 0), align 4
  %10 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 0, i32 1), align 4
  %11 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 0, i32 2), align 4
  %12 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 1, i32 0), align 4
  %13 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 1, i32 1), align 4
  %14 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 1, i32 2), align 4
  %15 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 2, i32 0), align 4
  %16 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 2, i32 1), align 4
  %17 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 2, i32 2), align 4
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @fmt.4, i32 0, i32 0), i32 %9, i32 %10, i32 %11, i32 %12, i32 %13, i32 %14, i32 %15, i32 %16, i32 %17)
  %p12 = load [2 x i32], [2 x i32]* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 2), align 4
  store [2 x i32] %p12, [2 x i32]* bitcast (i32** @0 to [2 x i32]*), align 4
  %18 = load i32*, i32** @0, align 8
  %19 = getelementptr inbounds i32, i32* %18, i32 0
  %i = load i32, i32* %19, align 4
  %20 = getelementptr inbounds i32, i32* %18, i32 1
  %j = load i32, i32* %20, align 4
  %board = getelementptr [3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 %i, i32 %j
  %board13 = load i32, i32* %board, align 4
  store i32 %board13, i32* @x, align 4
  %p14 = load [2 x i32], [2 x i32]* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 2), align 4
  store [2 x i32] %p14, [2 x i32]* bitcast (i32** @1 to [2 x i32]*), align 4
  %21 = load i32*, i32** @1, align 8
  %22 = getelementptr inbounds i32, i32* %21, i32 0
  %i15 = load i32, i32* %22, align 4
  %23 = getelementptr inbounds i32, i32* %21, i32 1
  %j16 = load i32, i32* %23, align 4
  %board17 = getelementptr [3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 %i15, i32 %j16
  store i32 0, i32* %board17, align 4
  %p18 = load [2 x i32], [2 x i32]* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 2), align 4
  store [2 x i32] %p18, [2 x i32]* bitcast (i32** @2 to [2 x i32]*), align 4
  %24 = load i32*, i32** @2, align 8
  %vector_119 = load i32*, i32** @vector_1, align 8
  %25 = getelementptr inbounds i32, i32* %24, i32 0
  %26 = load i32, i32* %25, align 4
  %27 = getelementptr inbounds i32, i32* %24, i32 1
  %28 = load i32, i32* %27, align 4
  %29 = getelementptr inbounds i32, i32* %vector_119, i32 0
  %30 = load i32, i32* %29, align 4
  %31 = getelementptr inbounds i32, i32* %vector_119, i32 1
  %32 = load i32, i32* %31, align 4
  %tmp20 = add i32 %26, %30
  %tmp21 = add i32 %28, %32
  %malloccall22 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %33 = bitcast i8* %malloccall22 to i32*
  %34 = getelementptr i32, i32* %33, i32 0
  store i32 %tmp20, i32* %34, align 4
  %35 = getelementptr i32, i32* %33, i32 1
  store i32 %tmp21, i32* %35, align 4
  store i32* %33, i32** bitcast ([2 x i32]* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 2) to i32**), align 8
  %x23 = load i32, i32* @x, align 4
  %p24 = load [2 x i32], [2 x i32]* getelementptr inbounds (%Player, %Player* @p, i32 0, i32 2), align 4
  store [2 x i32] %p24, [2 x i32]* bitcast (i32** @3 to [2 x i32]*), align 4
  %36 = load i32*, i32** @3, align 8
  %37 = getelementptr inbounds i32, i32* %36, i32 0
  %i25 = load i32, i32* %37, align 4
  %38 = getelementptr inbounds i32, i32* %36, i32 1
  %j26 = load i32, i32* %38, align 4
  %board27 = getelementptr [3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 %i25, i32 %j26
  store i32 %x23, i32* %board27, align 4
  %39 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 0, i32 0), align 4
  %40 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 0, i32 1), align 4
  %41 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 0, i32 2), align 4
  %42 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 1, i32 0), align 4
  %43 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 1, i32 1), align 4
  %44 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 1, i32 2), align 4
  %45 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 2, i32 0), align 4
  %46 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 2, i32 1), align 4
  %47 = load i32, i32* getelementptr inbounds ([3 x [3 x i32]], [3 x [3 x i32]]* @board, i32 0, i32 2, i32 2), align 4
  %printf28 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @fmt.5, i32 0, i32 0), i32 %39, i32 %40, i32 %41, i32 %42, i32 %43, i32 %44, i32 %45, i32 %46, i32 %47)
  ret i32 0

then:                                             ; preds = %entry
  %vector_1 = load i32*, i32** @vector_1, align 8
  %vector_2 = load i32*, i32** @vector_2, align 8
  %48 = getelementptr inbounds i32, i32* %vector_1, i32 0
  %49 = load i32, i32* %48, align 4
  %50 = getelementptr inbounds i32, i32* %vector_1, i32 1
  %51 = load i32, i32* %50, align 4
  %52 = getelementptr inbounds i32, i32* %vector_2, i32 0
  %53 = load i32, i32* %52, align 4
  %54 = getelementptr inbounds i32, i32* %vector_2, i32 1
  %55 = load i32, i32* %54, align 4
  %tmp4 = add i32 %49, %53
  %tmp5 = add i32 %51, %55
  %malloccall6 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %56 = bitcast i8* %malloccall6 to i32*
  %57 = getelementptr i32, i32* %56, i32 0
  store i32 %tmp4, i32* %57, align 4
  %58 = getelementptr i32, i32* %56, i32 1
  store i32 %tmp5, i32* %58, align 4
  store i32* %56, i32** @vector_1, align 8
  br label %merge

else:                                             ; preds = %entry
  %vector_17 = load i32*, i32** @vector_1, align 8
  %vector_28 = load i32*, i32** @vector_2, align 8
  %59 = getelementptr inbounds i32, i32* %vector_17, i32 0
  %60 = load i32, i32* %59, align 4
  %61 = getelementptr inbounds i32, i32* %vector_17, i32 1
  %62 = load i32, i32* %61, align 4
  %63 = getelementptr inbounds i32, i32* %vector_28, i32 0
  %64 = load i32, i32* %63, align 4
  %65 = getelementptr inbounds i32, i32* %vector_28, i32 1
  %66 = load i32, i32* %65, align 4
  %tmp9 = sub i32 %60, %64
  %tmp10 = sub i32 %62, %66
  %malloccall11 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %67 = bitcast i8* %malloccall11 to i32*
  %68 = getelementptr i32, i32* %67, i32 0
  store i32 %tmp9, i32* %68, align 4
  %69 = getelementptr i32, i32* %67, i32 1
  store i32 %tmp10, i32* %69, align 4
  store i32* %67, i32** @vector_1, align 8
  br label %merge
}

declare noalias i8* @malloc(i32)
