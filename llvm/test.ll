source_filename = "stdkraa"

attributes #0 = { alignstack=16 }

@argv = unnamed_addr global i64 0

define void @_start() naked {
       %rsp = call i64** asm inteldialect "mov rbx, rsp", "={rbx}" ()
       
       %argcp = bitcast i64** %rsp to i64*
       %argc = load i64, i64* %argcp
       
       %envp = add i64 %argc, 2
       
       %argvp = bitcast i64** %rsp to i8**
       %argv = getelementptr i8*, i8** %argvp, i64 %envp

       %ip = alloca i32
       store i32 0, i32* %ip
       %null = inttoptr i64 0 to i8*
       br label %args
       
args:
        %i = load i32, i32* %ip
        %elemp = getelementptr i8*, i8** %argv, i32 %i
        %elem = load i8*, i8** %elemp
        
        %ii = add i32 %i, 1
        store i32 %ii, i32* %ip
        
        %cond = icmp eq i8* %elem, %null
        br i1 %cond, label %exit, label %print
print:
        call void @println(i8* %elem)
        br label %args
        
exit:
       call void asm inteldialect "
            mov rax, 60
            syscall
            ", "{rdi}" (i64 %argc)
        ret void
}

define void @println(i8* %str) {
       %n = alloca i8
       store i8 10, i8* %n
       call void @print(i8* %str)
       call void @write(i64 1, i8* %n, i64 1)
       ret void
}

define void @print(i8* %str) {
       %len = call i64 @strlen(i8* %str)
       call void @write(i64 1, i8* %str, i64 %len)
       ret void
}

define i64 @strlen(i8* %str) {
       %ptrI = alloca i64
       store i64 0, i64* %ptrI
       br label %loop
loop:
       %i = load i64, i64* %ptrI
       %ii = add i64 %i, 1
       store i64 %ii, i64* %ptrI
       %nth = getelementptr i8, i8* %str, i64 %i
       %e = load i8, i8* %nth
       %cond = icmp eq i8 0, %e
       br i1 %cond, label %ret, label %loop
ret:
        ret i64 %i
}

define void @write(i64 %fd, i8* %buf, i64 %len) #0 {
       call void asm inteldialect "
            mov rax, 1
            syscall
       ", "{rdi},{rsi},{rdx}" (i64 %fd, i8* %buf, i64 %len)
       ret void
}

; define fastcc i32 @main(i32 %argc, i32 %argv) {
;        %cnt = alloca i32
;        store i32 6, i32* %cnt
;        br label %loop
       
; loop:
;         %x = call i32 @add(i32 %argv, i32 %argc)
;         %i = load i32, i32* %cnt
;         %ii = sub i32 %i, 1
;         store i32 %ii, i32* %cnt
;         %cond = icmp sge i32 %ii, 0
;         br i1 %cond, label %loop, label %ret
; ret:
;         ret i32 %x
; }

; define i32 @add(i32 %a, i32 %b) alwaysinline {
;        %x = add i32 %a, %b
;        ret i32 %x
; }

