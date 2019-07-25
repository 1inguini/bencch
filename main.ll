define i32 @main() {
  %1 = alloca i32, align 4
  store i32 5, i32* %1, align 4

  %2 = alloca i32, align 4
  store i32 3, i32* %2, align 4

  %3 = load i32, i32* %1, align 4
  %4 = load i32, i32* %2, align 4
  %5 = sub nsw i32 %3, %4
  
  %6 = alloca i32, align 4  
  store i32 %5, i32* %6, align 4

  %7 = load i32, i32* %6, align 4
  ret i32 %7
}

; define i32 @main() {
;   %1 = alloca i32, align 4
;   store i32 4, i32* %1, align 4
  
;   %2 = alloca i32, align 4
;   store i32 12, i32* %2, align 4
  
;   %3 = load i32, i32* %1, align 4
;   %4 = load i32, i32* %2, align 4
;   %5 = add nsw i32 %3, %4

;   %6 = alloca i32, align 4
;   store i32 %5, i32* %6, align 4
  
;   %7 = load i32, i32* %6, align 4
;   ret i32 %7
; }
