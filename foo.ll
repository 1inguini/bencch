; ModuleID = 'foo.c'
source_filename = "foo.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@.str = private unnamed_addr constant [21 x i8] c"function foo called\0A\00", align 1
@.str.1 = private unnamed_addr constant [116 x i8] c"bar(%ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld, %ld), sum = %ld\0A\00", align 1
@.str.2 = private unnamed_addr constant [5 x i8] c"%ld\0A\00", align 1

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i64 @foo() #0 {
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str, i32 0, i32 0))
  ret i64 100
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i64 @bar(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) #0 {
  %21 = alloca i64, align 8
  %22 = alloca i64, align 8
  %23 = alloca i64, align 8
  %24 = alloca i64, align 8
  %25 = alloca i64, align 8
  %26 = alloca i64, align 8
  %27 = alloca i64, align 8
  %28 = alloca i64, align 8
  %29 = alloca i64, align 8
  %30 = alloca i64, align 8
  %31 = alloca i64, align 8
  %32 = alloca i64, align 8
  %33 = alloca i64, align 8
  %34 = alloca i64, align 8
  %35 = alloca i64, align 8
  %36 = alloca i64, align 8
  %37 = alloca i64, align 8
  %38 = alloca i64, align 8
  %39 = alloca i64, align 8
  %40 = alloca i64, align 8
  %41 = alloca i64, align 8
  store i64 %0, i64* %21, align 8
  store i64 %1, i64* %22, align 8
  store i64 %2, i64* %23, align 8
  store i64 %3, i64* %24, align 8
  store i64 %4, i64* %25, align 8
  store i64 %5, i64* %26, align 8
  store i64 %6, i64* %27, align 8
  store i64 %7, i64* %28, align 8
  store i64 %8, i64* %29, align 8
  store i64 %9, i64* %30, align 8
  store i64 %10, i64* %31, align 8
  store i64 %11, i64* %32, align 8
  store i64 %12, i64* %33, align 8
  store i64 %13, i64* %34, align 8
  store i64 %14, i64* %35, align 8
  store i64 %15, i64* %36, align 8
  store i64 %16, i64* %37, align 8
  store i64 %17, i64* %38, align 8
  store i64 %18, i64* %39, align 8
  store i64 %19, i64* %40, align 8
  %42 = load i64, i64* %21, align 8
  %43 = load i64, i64* %22, align 8
  %44 = add nsw i64 %42, %43
  %45 = load i64, i64* %23, align 8
  %46 = add nsw i64 %44, %45
  %47 = load i64, i64* %24, align 8
  %48 = add nsw i64 %46, %47
  %49 = load i64, i64* %25, align 8
  %50 = add nsw i64 %48, %49
  %51 = load i64, i64* %26, align 8
  %52 = add nsw i64 %50, %51
  %53 = load i64, i64* %27, align 8
  %54 = add nsw i64 %52, %53
  %55 = load i64, i64* %28, align 8
  %56 = add nsw i64 %54, %55
  %57 = load i64, i64* %29, align 8
  %58 = add nsw i64 %56, %57
  %59 = load i64, i64* %30, align 8
  %60 = add nsw i64 %58, %59
  %61 = load i64, i64* %31, align 8
  %62 = add nsw i64 %60, %61
  %63 = load i64, i64* %32, align 8
  %64 = add nsw i64 %62, %63
  %65 = load i64, i64* %33, align 8
  %66 = add nsw i64 %64, %65
  %67 = load i64, i64* %34, align 8
  %68 = add nsw i64 %66, %67
  %69 = load i64, i64* %35, align 8
  %70 = add nsw i64 %68, %69
  %71 = load i64, i64* %36, align 8
  %72 = add nsw i64 %70, %71
  %73 = load i64, i64* %37, align 8
  %74 = add nsw i64 %72, %73
  %75 = load i64, i64* %38, align 8
  %76 = add nsw i64 %74, %75
  %77 = load i64, i64* %39, align 8
  %78 = add nsw i64 %76, %77
  %79 = load i64, i64* %40, align 8
  %80 = add nsw i64 %78, %79
  store i64 %80, i64* %41, align 8
  %81 = load i64, i64* %21, align 8
  %82 = load i64, i64* %22, align 8
  %83 = load i64, i64* %23, align 8
  %84 = load i64, i64* %24, align 8
  %85 = load i64, i64* %25, align 8
  %86 = load i64, i64* %26, align 8
  %87 = load i64, i64* %27, align 8
  %88 = load i64, i64* %28, align 8
  %89 = load i64, i64* %29, align 8
  %90 = load i64, i64* %30, align 8
  %91 = load i64, i64* %31, align 8
  %92 = load i64, i64* %32, align 8
  %93 = load i64, i64* %33, align 8
  %94 = load i64, i64* %34, align 8
  %95 = load i64, i64* %35, align 8
  %96 = load i64, i64* %36, align 8
  %97 = load i64, i64* %37, align 8
  %98 = load i64, i64* %38, align 8
  %99 = load i64, i64* %39, align 8
  %100 = load i64, i64* %40, align 8
  %101 = load i64, i64* %41, align 8
  %102 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([116 x i8], [116 x i8]* @.str.1, i32 0, i32 0), i64 %81, i64 %82, i64 %83, i64 %84, i64 %85, i64 %86, i64 %87, i64 %88, i64 %89, i64 %90, i64 %91, i64 %92, i64 %93, i64 %94, i64 %95, i64 %96, i64 %97, i64 %98, i64 %99, i64 %100, i64 %101)
  %103 = sext i32 %102 to i64
  ret i64 %103
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local i64 @show_long(i64) #0 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.2, i32 0, i32 0), i64 %3)
  %5 = load i64, i64* %2, align 8
  ret i64 %5
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define dso_local void @alloc4(i64**, i64, i64, i64, i64) #0 {
  %6 = alloca i64**, align 8
  %7 = alloca i64, align 8
  %8 = alloca i64, align 8
  %9 = alloca i64, align 8
  %10 = alloca i64, align 8
  store i64** %0, i64*** %6, align 8
  store i64 %1, i64* %7, align 8
  store i64 %2, i64* %8, align 8
  store i64 %3, i64* %9, align 8
  store i64 %4, i64* %10, align 8
  %11 = call noalias i8* @malloc(i64 16) #3
  %12 = bitcast i8* %11 to i64*
  %13 = load i64**, i64*** %6, align 8
  store i64* %12, i64** %13, align 8
  %14 = load i64, i64* %7, align 8
  %15 = load i64**, i64*** %6, align 8
  %16 = load i64*, i64** %15, align 8
  %17 = getelementptr inbounds i64, i64* %16, i64 0
  store i64 %14, i64* %17, align 8
  %18 = call noalias i8* @malloc(i64 16) #3
  %19 = bitcast i8* %18 to i64*
  %20 = load i64**, i64*** %6, align 8
  store i64* %19, i64** %20, align 8
  %21 = load i64, i64* %8, align 8
  %22 = load i64**, i64*** %6, align 8
  %23 = load i64*, i64** %22, align 8
  %24 = getelementptr inbounds i64, i64* %23, i64 0
  store i64 %21, i64* %24, align 8
  %25 = call noalias i8* @malloc(i64 16) #3
  %26 = bitcast i8* %25 to i64*
  %27 = load i64**, i64*** %6, align 8
  store i64* %26, i64** %27, align 8
  %28 = load i64, i64* %9, align 8
  %29 = load i64**, i64*** %6, align 8
  %30 = load i64*, i64** %29, align 8
  %31 = getelementptr inbounds i64, i64* %30, i64 0
  store i64 %28, i64* %31, align 8
  %32 = call noalias i8* @malloc(i64 16) #3
  %33 = bitcast i8* %32 to i64*
  %34 = load i64**, i64*** %6, align 8
  store i64* %33, i64** %34, align 8
  %35 = load i64, i64* %10, align 8
  %36 = load i64**, i64*** %6, align 8
  %37 = load i64*, i64** %36, align 8
  %38 = getelementptr inbounds i64, i64* %37, i64 0
  store i64 %35, i64* %38, align 8
  ret void
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #2

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 8.0.1 (tags/RELEASE_801/final)"}
