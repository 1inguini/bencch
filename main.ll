; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @example() {
  %1 = alloca i64, align 8
  %2 = alloca i64*, align 8
  %3 = load i64, i64* %1, align 8
  %4 = load i64*, i64** %2, align 8
  store i64* %1, i64** %2, align 8
  %5 = load i64*, i64** %2, align 8
  %6 = load i64, i64* %5, align 8
  %7 = load i64*, i64** %2, align 8
  store i64 3, i64* %7, align 8
  %8 = load i64, i64* %1, align 8
  ret i64 %8
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i32 @main() {
  %1 = alloca i32, align 4
  %2 = alloca i64, align 8
  store i32 0, i32* %1, align 4
  store i64 0, i64* %2, align 8
  br label %3

; <label>:3:                                      ; preds = %9, %0
  %4 = load i64, i64* %2, align 8
  %5 = icmp slt i64 %4, 1000000000
  br i1 %5, label %6, label %12

; <label>:6:                                      ; preds = %3
  %7 = load i64, i64* %2, align 8
  %8 = call i32 (i64, ...) bitcast (i32 (...)* @show_long to i32 (i64, ...)*)(i64 %7)
  br label %9

; <label>:9:                                      ; preds = %6
  %10 = load i64, i64* %2, align 8
  %11 = add nsw i64 %10, 1
  store i64 %11, i64* %2, align 8
  br label %3

; <label>:12:                                     ; preds = %3
  ret i32 0
}

declare i32 @show_long(...) #1
