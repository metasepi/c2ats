// File: example.h
%{#
#include <stdio.h>

%}
abst@ype type_c2ats___gnuc_va_list
abst@ype type_c2ats___any
viewdef ptr_v_1 (a:t@ype, l:addr) = a @ l
dataview ptr_v_2 (a:t@ype+, l0: addr, l1: addr) =
  | ptr_v_2_cons(a, l0, l1) of (ptr l1 @ l0, ptr_v_1 (a, l1))
dataview ptr_v_3 (a:t@ype+, l0:addr, l1:addr, l2:addr) =
  | ptr_v_3_cons(a, l0, l1, l2) of (ptr l1 @ l0, ptr_v_2 (a, l1, l2))
// No file
fun fun_c2ats___builtin_extract_return_addr: (ptr) -> ptr = "mac#__builtin_extract_return_addr"
macdef takeout_c2ats___FUNCTION__ = $extval([l1:addr] (ptr_v_1(@[char][0], l1) | ptr l1), "&__FUNCTION__")
praxi addback_c2ats___FUNCTION__ {l1:addr} (ptr_v_1(@[char][0], l1) | ptr l1): void
fun fun_c2ats___builtin___snprintf_chk: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, int, int, int, ptr l2) -> int = "mac#__builtin___snprintf_chk"
fun fun_c2ats___builtin___vsprintf_chk: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, int, int, ptr l2, type_c2ats___gnuc_va_list) -> int = "mac#__builtin___vsprintf_chk"
fun fun_c2ats___builtin___memcpy_chk: (ptr, ptr, int, int) -> ptr = "mac#__builtin___memcpy_chk"
fun fun_c2ats___builtin___stpcpy_chk: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, int) -> [l3:addr] (ptr_v_1(char, l3) | ptr l3) = "mac#__builtin___stpcpy_chk"
fun fun_c2ats___builtin___strcat_chk: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, int) -> [l3:addr] (ptr_v_1(char, l3) | ptr l3) = "mac#__builtin___strcat_chk"
fun fun_c2ats___builtin___strcpy_chk: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, int) -> [l3:addr] (ptr_v_1(char, l3) | ptr l3) = "mac#__builtin___strcpy_chk"
fun fun_c2ats___builtin___sprintf_chk: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, int, int, ptr l2) -> int = "mac#__builtin___sprintf_chk"
fun fun_c2ats___builtin_return_address: (uint) -> ptr = "mac#__builtin_return_address"
fun fun_c2ats___builtin_va_arg_pack: () -> int = "mac#__builtin_va_arg_pack"
fun fun_c2ats___builtin___memmove_chk: (ptr, ptr, int, int) -> ptr = "mac#__builtin___memmove_chk"
fun fun_c2ats___builtin___vsnprintf_chk: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, int, int, int, ptr l2, type_c2ats___gnuc_va_list) -> int = "mac#__builtin___vsnprintf_chk"
fun fun_c2ats___builtin___strncat_chk: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, int, int) -> [l3:addr] (ptr_v_1(char, l3) | ptr l3) = "mac#__builtin___strncat_chk"
fun fun_c2ats___builtin___strncpy_chk: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, int, int) -> [l3:addr] (ptr_v_1(char, l3) | ptr l3) = "mac#__builtin___strncpy_chk"
fun fun_c2ats___builtin___mempcpy_chk: (ptr, ptr, int, int) -> ptr = "mac#__builtin___mempcpy_chk"
fun fun_c2ats___builtin___memset_chk: (ptr, int, int, int) -> ptr = "mac#__builtin___memset_chk"
fun fun_c2ats___builtin_constant_p: (type_c2ats___any) -> int = "mac#__builtin_constant_p"
fun fun_c2ats___builtin_va_start: (type_c2ats___gnuc_va_list, ptr) -> void = "mac#__builtin_va_start"
fun fun_c2ats___builtin_frame_address: (uint) -> ptr = "mac#__builtin_frame_address"
fun fun_c2ats___builtin_va_end: (type_c2ats___gnuc_va_list) -> void = "mac#__builtin_va_end"
fun fun_c2ats___builtin_alloca: (int) -> ptr = "mac#__builtin_alloca"
fun fun_c2ats___builtin_object_size: (ptr, int) -> int = "mac#__builtin_object_size"
fun fun_c2ats___builtin_va_copy: (type_c2ats___gnuc_va_list, type_c2ats___gnuc_va_list) -> void = "mac#__builtin_va_copy"
fun fun_c2ats___builtin_strncat: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, int) -> [l3:addr] (ptr_v_1(char, l3) | ptr l3) = "mac#__builtin_strncat"
fun fun_c2ats___builtin_copysign: (double, double) -> double = "mac#__builtin_copysign"
fun fun_c2ats___builtin_memcpy: (ptr, ptr, int) -> ptr = "mac#__builtin_memcpy"
fun fun_c2ats___builtin_fabs: (double) -> double = "mac#__builtin_fabs"
fun fun_c2ats___builtin_fabsf: (float) -> float = "mac#__builtin_fabsf"
fun fun_c2ats___builtin_fabsl: (ldouble) -> ldouble = "mac#__builtin_fabsl"
fun fun_c2ats___builtin_strspn: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> int = "mac#__builtin_strspn"
fun fun_c2ats___builtin_strncpy: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, int) -> [l3:addr] (ptr_v_1(char, l3) | ptr l3) = "mac#__builtin_strncpy"
fun fun_c2ats___builtin_strcmp: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> int = "mac#__builtin_strcmp"
fun fun_c2ats___builtin_strcspn: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> int = "mac#__builtin_strcspn"
fun fun_c2ats___builtin_strpbrk: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> [l3:addr] (ptr_v_1(char, l3) | ptr l3) = "mac#__builtin_strpbrk"
fun fun_c2ats___builtin_prefetch: (ptr) -> void = "mac#__builtin_prefetch"
fun fun_c2ats___builtin_strchr: {l1:addr} (!ptr_v_1(char, l1) | ptr l1, int) -> [l2:addr] (ptr_v_1(char, l2) | ptr l2) = "mac#__builtin_strchr"
macdef takeout_c2ats___PRETTY_FUNCTION__ = $extval([l1:addr] (ptr_v_1(@[char][0], l1) | ptr l1), "&__PRETTY_FUNCTION__")
praxi addback_c2ats___PRETTY_FUNCTION__ {l1:addr} (ptr_v_1(@[char][0], l1) | ptr l1): void
fun fun_c2ats___builtin_huge_val: () -> double = "mac#__builtin_huge_val"
fun fun_c2ats___builtin_clz: (uint) -> int = "mac#__builtin_clz"
fun fun_c2ats___builtin_huge_valf: () -> float = "mac#__builtin_huge_valf"
fun fun_c2ats___builtin_huge_vall: () -> ldouble = "mac#__builtin_huge_vall"
fun fun_c2ats___builtin_expect: (lint, lint) -> lint = "mac#__builtin_expect"
fun fun_c2ats___builtin_inf: () -> double = "mac#__builtin_inf"
fun fun_c2ats___builtin_inff: () -> float = "mac#__builtin_inff"
fun fun_c2ats___builtin_infl: () -> ldouble = "mac#__builtin_infl"
macdef takeout_c2ats___func__ = $extval([l1:addr] (ptr_v_1(@[char][0], l1) | ptr l1), "&__func__")
praxi addback_c2ats___func__ {l1:addr} (ptr_v_1(@[char][0], l1) | ptr l1): void
fun fun_c2ats___builtin_bzero: (ptr, int) -> void = "mac#__builtin_bzero"
fun fun_c2ats___builtin_va_arg_pack_len: () -> int = "mac#__builtin_va_arg_pack_len"
typedef type_c2ats___builtin_va_list = type_c2ats___gnuc_va_list
// File: /usr/lib/gcc/x86_64-linux-gnu/6/include/stddef.h
typedef type_c2ats_size_t = ulint
// File: /usr/include/x86_64-linux-gnu/bits/types.h
typedef type_c2ats___u_char = uchar
typedef type_c2ats___u_short = usint
typedef type_c2ats___u_int = uint
typedef type_c2ats___u_long = ulint
typedef type_c2ats___int8_t = schar
typedef type_c2ats___uint8_t = uchar
typedef type_c2ats___int16_t = sint
typedef type_c2ats___uint16_t = usint
typedef type_c2ats___int32_t = int
typedef type_c2ats___uint32_t = uint
typedef type_c2ats___int64_t = lint
typedef type_c2ats___uint64_t = ulint
typedef type_c2ats___quad_t = lint
typedef type_c2ats___u_quad_t = ulint
typedef type_c2ats___dev_t = ulint
typedef type_c2ats___uid_t = uint
typedef type_c2ats___gid_t = uint
typedef type_c2ats___ino_t = ulint
typedef type_c2ats___ino64_t = ulint
typedef type_c2ats___mode_t = uint
typedef type_c2ats___nlink_t = ulint
typedef type_c2ats___off_t = lint
typedef type_c2ats___off64_t = lint
typedef type_c2ats___pid_t = int
typedef struct_c2ats_anon_169 = $extype_struct"struct { int __val[2]; }" of {
  __val = @[int][2]
}
typedef type_c2ats___fsid_t = struct_c2ats_anon_169
typedef type_c2ats___clock_t = lint
typedef type_c2ats___rlim_t = ulint
typedef type_c2ats___rlim64_t = ulint
typedef type_c2ats___id_t = uint
typedef type_c2ats___time_t = lint
typedef type_c2ats___useconds_t = uint
typedef type_c2ats___suseconds_t = lint
typedef type_c2ats___daddr_t = int
typedef type_c2ats___key_t = int
typedef type_c2ats___clockid_t = int
typedef type_c2ats___timer_t = ptr
typedef type_c2ats___blksize_t = lint
typedef type_c2ats___blkcnt_t = lint
typedef type_c2ats___blkcnt64_t = lint
typedef type_c2ats___fsblkcnt_t = ulint
typedef type_c2ats___fsblkcnt64_t = ulint
typedef type_c2ats___fsfilcnt_t = ulint
typedef type_c2ats___fsfilcnt64_t = ulint
typedef type_c2ats___fsword_t = lint
typedef type_c2ats___ssize_t = lint
typedef type_c2ats___syscall_slong_t = lint
typedef type_c2ats___syscall_ulong_t = ulint
typedef type_c2ats___loff_t = type_c2ats___off64_t
typedef type_c2ats___qaddr_t = cPtr0(type_c2ats___quad_t)
typedef type_c2ats___caddr_t = cPtr0(char)
typedef type_c2ats___intptr_t = lint
typedef type_c2ats___socklen_t = uint
// File: /usr/include/stdio.h
abst@ype struct_c2ats__IO_FILE // FIXME! Forward declaration.
typedef type_c2ats_FILE = struct_c2ats__IO_FILE
typedef type_c2ats___FILE = struct_c2ats__IO_FILE
abst@ype type_c2ats___gnuc_va_list // FIXME! Forward declaration.
typedef type_c2ats_va_list = type_c2ats___gnuc_va_list
abst@ype type_c2ats__G_fpos_t // FIXME! Forward declaration.
typedef type_c2ats_fpos_t = type_c2ats__G_fpos_t
macdef takeout_c2ats_stdin = $extval([l1,l1_1:addr] (ptr_v_2(struct_c2ats__IO_FILE, l1, l1_1) | ptr l1), "&stdin")
praxi addback_c2ats_stdin {l1,l1_1:addr} (ptr_v_2(struct_c2ats__IO_FILE, l1, l1_1) | ptr l1): void
macdef takeout_c2ats_stdout = $extval([l1,l1_1:addr] (ptr_v_2(struct_c2ats__IO_FILE, l1, l1_1) | ptr l1), "&stdout")
praxi addback_c2ats_stdout {l1,l1_1:addr} (ptr_v_2(struct_c2ats__IO_FILE, l1, l1_1) | ptr l1): void
macdef takeout_c2ats_stderr = $extval([l1,l1_1:addr] (ptr_v_2(struct_c2ats__IO_FILE, l1, l1_1) | ptr l1), "&stderr")
praxi addback_c2ats_stderr {l1,l1_1:addr} (ptr_v_2(struct_c2ats__IO_FILE, l1, l1_1) | ptr l1): void
fun fun_c2ats_remove: {l1:addr} (!ptr_v_1(char, l1) | ptr l1) -> int = "mac#remove"
fun fun_c2ats_rename: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> int = "mac#rename"
fun fun_c2ats_tmpfile: () -> [l1:addr] (ptr_v_1(type_c2ats_FILE, l1) | ptr l1) = "mac#tmpfile"
fun fun_c2ats_tmpnam: {l1:addr} (!ptr_v_1(char, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2) | ptr l2) = "mac#tmpnam"
fun fun_c2ats_tempnam: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> [l3:addr] (ptr_v_1(char, l3) | ptr l3) = "mac#tempnam"
fun fun_c2ats_fclose: {l1:addr} (ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> int = "mac#fclose"
fun fun_c2ats_fflush: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> int = "mac#fflush"
fun fun_c2ats_fopen: (string, string) -> [l3:addr] (ptr_v_1(type_c2ats_FILE, l3) | ptr l3) = "mac#fopen"
fun fun_c2ats_freopen: {l1,l2,l3:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2), !ptr_v_1(type_c2ats_FILE, l3) | ptr l1, ptr l2, ptr l3) -> [l4:addr] (ptr_v_1(type_c2ats_FILE, l4) | ptr l4) = "mac#freopen"
fun fun_c2ats_fdopen: {l1:addr} (!ptr_v_1(char, l1) | int, ptr l1) -> [l2:addr] (ptr_v_1(type_c2ats_FILE, l2) | ptr l2) = "mac#fdopen"
fun fun_c2ats_setbuf: {l1,l2:addr} (!ptr_v_1(type_c2ats_FILE, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> void = "mac#setbuf"
fun fun_c2ats_setvbuf: {l1,l2:addr} (!ptr_v_1(type_c2ats_FILE, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, int, type_c2ats_size_t) -> int = "mac#setvbuf"
fun fun_c2ats_fprintf: {l1,l2:addr} (!ptr_v_1(type_c2ats_FILE, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> int = "mac#fprintf"
fun fun_c2ats_printf: {l1:addr} (!ptr_v_1(char, l1) | ptr l1) -> int = "mac#printf"
fun fun_c2ats_sprintf: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> int = "mac#sprintf"
fun fun_c2ats_vfprintf: {l1,l2:addr} (!ptr_v_1(type_c2ats_FILE, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, type_c2ats___gnuc_va_list) -> int = "mac#vfprintf"
fun fun_c2ats_vprintf: {l1:addr} (!ptr_v_1(char, l1) | ptr l1, type_c2ats___gnuc_va_list) -> int = "mac#vprintf"
fun fun_c2ats_vsprintf: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, type_c2ats___gnuc_va_list) -> int = "mac#vsprintf"
fun fun_c2ats_snprintf: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, type_c2ats_size_t, ptr l2) -> int = "mac#snprintf"
fun fun_c2ats_vsnprintf: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, type_c2ats_size_t, ptr l2, type_c2ats___gnuc_va_list) -> int = "mac#vsnprintf"
fun fun_c2ats_fscanf: {l1,l2:addr} (!ptr_v_1(type_c2ats_FILE, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> int = "mac#fscanf"
fun fun_c2ats_scanf: {l1:addr} (!ptr_v_1(char, l1) | ptr l1) -> int = "mac#scanf"
fun fun_c2ats_sscanf: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> int = "mac#sscanf"
fun fun_c2ats_vfscanf: {l1,l2:addr} (!ptr_v_1(type_c2ats_FILE, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, type_c2ats___gnuc_va_list) -> int = "mac#vfscanf"
fun fun_c2ats_vscanf: {l1:addr} (!ptr_v_1(char, l1) | ptr l1, type_c2ats___gnuc_va_list) -> int = "mac#vscanf"
fun fun_c2ats_vsscanf: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, type_c2ats___gnuc_va_list) -> int = "mac#vsscanf"
fun fun_c2ats_fgetc: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> int = "mac#fgetc"
fun fun_c2ats_getc: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> int = "mac#getc"
fun fun_c2ats_getchar: () -> int = "mac#getchar"
fun fun_c2ats_fputc: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | int, ptr l1) -> int = "mac#fputc"
fun fun_c2ats_putc: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | int, ptr l1) -> int = "mac#putc"
fun fun_c2ats_putchar: (int) -> int = "mac#putchar"
fun fun_c2ats_getw: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> int = "mac#getw"
fun fun_c2ats_putw: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | int, ptr l1) -> int = "mac#putw"
fun fun_c2ats_fgets: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(type_c2ats_FILE, l2) | ptr l1, int, ptr l2) -> [l3:addr] (ptr_v_1(char, l3) | ptr l3) = "mac#fgets"
fun fun_c2ats_fputs: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(type_c2ats_FILE, l2) | ptr l1, ptr l2) -> int = "mac#fputs"
fun fun_c2ats_puts: {l1:addr} (!ptr_v_1(char, l1) | ptr l1) -> int = "mac#puts"
fun fun_c2ats_ungetc: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | int, ptr l1) -> int = "mac#ungetc"
fun fun_c2ats_fread: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | !strptr, type_c2ats_size_t, size_t, ptr l1) -> size_t = "mac#fread"
fun fun_c2ats_fwrite: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr, type_c2ats_size_t, type_c2ats_size_t, ptr l1) -> type_c2ats_size_t = "mac#fwrite"
fun fun_c2ats_fseek: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1, lint, int) -> int = "mac#fseek"
fun fun_c2ats_ftell: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> lint = "mac#ftell"
fun fun_c2ats_rewind: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> void = "mac#rewind"
fun fun_c2ats_fgetpos: {l1,l2:addr} (!ptr_v_1(type_c2ats_FILE, l1), !ptr_v_1(type_c2ats_fpos_t, l2) | ptr l1, ptr l2) -> int = "mac#fgetpos"
fun fun_c2ats_fsetpos: {l1,l2:addr} (!ptr_v_1(type_c2ats_FILE, l1), !ptr_v_1(type_c2ats_fpos_t, l2) | ptr l1, ptr l2) -> int = "mac#fsetpos"
fun fun_c2ats_clearerr: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> void = "mac#clearerr"
fun fun_c2ats_feof: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> int = "mac#feof"
fun fun_c2ats_ferror: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> int = "mac#ferror"
fun fun_c2ats_perror: {l1:addr} (!ptr_v_1(char, l1) | ptr l1) -> void = "mac#perror"
fun fun_c2ats_fileno: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> int = "mac#fileno"
fun fun_c2ats_popen: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> [l3:addr] (ptr_v_1(type_c2ats_FILE, l3) | ptr l3) = "mac#popen"
fun fun_c2ats_pclose: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr l1) -> int = "mac#pclose"
fun fun_c2ats_ctermid: {l1:addr} (!ptr_v_1(char, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2) | ptr l2) = "mac#ctermid"
fun fun_c2ats_cuserid: {l1:addr} (!ptr_v_1(char, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2) | ptr l2) = "mac#cuserid"
// File: /usr/include/wchar.h
typedef union_c2ats_anon_375 = $extype_struct"union { unsigned int __wch; char __wchb[4]; }" of {
  __wch = uint,
  __wchb = @[char][4]
}
typedef struct_c2ats_anon_380 = $extype_struct"struct { int __count; union { unsigned int __wch; char __wchb[4]; } __value; }" of {
  __count = int,
  __value = union_c2ats_anon_375
}
typedef type_c2ats___mbstate_t = struct_c2ats_anon_380
// File: /usr/include/_G_config.h
typedef struct_c2ats_anon_396 = $extype_struct"struct { __off_t __pos; __mbstate_t __state; }" of {
  __pos = type_c2ats___off_t,
  __state = type_c2ats___mbstate_t
}
typedef type_c2ats__G_fpos_t = struct_c2ats_anon_396
typedef struct_c2ats_anon_412 = $extype_struct"struct { __off64_t __pos; __mbstate_t __state; }" of {
  __pos = type_c2ats___off64_t,
  __state = type_c2ats___mbstate_t
}
typedef type_c2ats__G_fpos64_t = struct_c2ats_anon_412
// File: /usr/lib/gcc/x86_64-linux-gnu/6/include/stdarg.h
typedef type_c2ats___gnuc_va_list = type_c2ats___builtin_va_list
// File: /usr/include/libio.h
typedef type_c2ats__IO_lock_t = void
typedef struct_c2ats__IO_marker = $extype_struct"struct _IO_marker" of {
  _next = ptr (* cPtr0(struct_c2ats__IO_marker) *),
  _sbuf = ptr (* cPtr0(struct_c2ats__IO_FILE) *),
  _pos = int
}
#define enum_c2ats___codecvt_ok 0
#define enum_c2ats___codecvt_partial 1
#define enum_c2ats___codecvt_error 2
#define enum_c2ats___codecvt_noconv 3
typedef struct_c2ats__IO_FILE = $extype_struct"struct _IO_FILE" of {
  _flags = int,
  _IO_read_ptr = ptr (* cPtr0(char) *),
  _IO_read_end = ptr (* cPtr0(char) *),
  _IO_read_base = ptr (* cPtr0(char) *),
  _IO_write_base = ptr (* cPtr0(char) *),
  _IO_write_ptr = ptr (* cPtr0(char) *),
  _IO_write_end = ptr (* cPtr0(char) *),
  _IO_buf_base = ptr (* cPtr0(char) *),
  _IO_buf_end = ptr (* cPtr0(char) *),
  _IO_save_base = ptr (* cPtr0(char) *),
  _IO_backup_base = ptr (* cPtr0(char) *),
  _IO_save_end = ptr (* cPtr0(char) *),
  _markers = ptr (* cPtr0(struct_c2ats__IO_marker) *),
  _chain = ptr (* cPtr0(struct_c2ats__IO_FILE) *),
  _fileno = int,
  _flags2 = int,
  _old_offset = type_c2ats___off_t,
  _cur_column = usint,
  _vtable_offset = schar,
  _shortbuf = @[char][1],
  _lock = ptr (* cPtr0(type_c2ats__IO_lock_t) *),
  _offset = type_c2ats___off64_t,
  __pad1 = ptr (* cPtr0(void) *),
  __pad2 = ptr (* cPtr0(void) *),
  __pad3 = ptr (* cPtr0(void) *),
  __pad4 = ptr (* cPtr0(void) *),
  __pad5 = type_c2ats_size_t,
  _mode = int,
  _unused2 = @[char][15 * sizeof(int) - 4 * sizeof(ptr) - sizeof(type_c2ats_size_t)]
}
typedef type_c2ats__IO_FILE = struct_c2ats__IO_FILE
abst@ype struct_c2ats__IO_FILE_plus // FIXME! Forward declaration.
macdef takeout_c2ats__IO_2_1_stdin_ = $extval([l1:addr] (ptr_v_1(struct_c2ats__IO_FILE_plus, l1) | ptr l1), "&_IO_2_1_stdin_")
praxi addback_c2ats__IO_2_1_stdin_ {l1:addr} (ptr_v_1(struct_c2ats__IO_FILE_plus, l1) | ptr l1): void
macdef takeout_c2ats__IO_2_1_stdout_ = $extval([l1:addr] (ptr_v_1(struct_c2ats__IO_FILE_plus, l1) | ptr l1), "&_IO_2_1_stdout_")
praxi addback_c2ats__IO_2_1_stdout_ {l1:addr} (ptr_v_1(struct_c2ats__IO_FILE_plus, l1) | ptr l1): void
macdef takeout_c2ats__IO_2_1_stderr_ = $extval([l1:addr] (ptr_v_1(struct_c2ats__IO_FILE_plus, l1) | ptr l1), "&_IO_2_1_stderr_")
praxi addback_c2ats__IO_2_1_stderr_ {l1:addr} (ptr_v_1(struct_c2ats__IO_FILE_plus, l1) | ptr l1): void
typedef type_c2ats___io_read_fn = {l1:addr} (!ptr_v_1(char, l1) | ptr, ptr l1, type_c2ats_size_t) -> type_c2ats___ssize_t
typedef type_c2ats___io_write_fn = {l1:addr} (!ptr_v_1(char, l1) | ptr, ptr l1, type_c2ats_size_t) -> type_c2ats___ssize_t
typedef type_c2ats___io_seek_fn = {l1:addr} (!ptr_v_1(type_c2ats___off64_t, l1) | ptr, ptr l1, int) -> int
typedef type_c2ats___io_close_fn = (ptr) -> int
fun fun_c2ats___underflow: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1) -> int = "mac#__underflow"
fun fun_c2ats___uflow: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1) -> int = "mac#__uflow"
fun fun_c2ats___overflow: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1, int) -> int = "mac#__overflow"
fun fun_c2ats__IO_getc: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1) -> int = "mac#_IO_getc"
fun fun_c2ats__IO_putc: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | int, ptr l1) -> int = "mac#_IO_putc"
fun fun_c2ats__IO_feof: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1) -> int = "mac#_IO_feof"
fun fun_c2ats__IO_ferror: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1) -> int = "mac#_IO_ferror"
fun fun_c2ats__IO_peekc_locked: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1) -> int = "mac#_IO_peekc_locked"
fun fun_c2ats__IO_flockfile: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1) -> void = "mac#_IO_flockfile"
fun fun_c2ats__IO_funlockfile: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1) -> void = "mac#_IO_funlockfile"
fun fun_c2ats__IO_ftrylockfile: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1) -> int = "mac#_IO_ftrylockfile"
fun fun_c2ats__IO_vfscanf: {l1,l2,l3:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1), !ptr_v_1(char, l2), !ptr_v_1(int, l3) | ptr l1, ptr l2, type_c2ats___gnuc_va_list, ptr l3) -> int = "mac#_IO_vfscanf"
fun fun_c2ats__IO_vfprintf: {l1,l2:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2, type_c2ats___gnuc_va_list) -> int = "mac#_IO_vfprintf"
fun fun_c2ats__IO_padn: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1, int, type_c2ats___ssize_t) -> type_c2ats___ssize_t = "mac#_IO_padn"
fun fun_c2ats__IO_sgetn: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1, ptr, type_c2ats_size_t) -> type_c2ats_size_t = "mac#_IO_sgetn"
fun fun_c2ats__IO_seekoff: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1, type_c2ats___off64_t, int, int) -> type_c2ats___off64_t = "mac#_IO_seekoff"
fun fun_c2ats__IO_seekpos: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1, type_c2ats___off64_t, int) -> type_c2ats___off64_t = "mac#_IO_seekpos"
fun fun_c2ats__IO_free_backup_area: {l1:addr} (!ptr_v_1(type_c2ats__IO_FILE, l1) | ptr l1) -> void = "mac#_IO_free_backup_area"
// File: /usr/include/getopt.h
macdef takeout_c2ats_optarg = $extval([l1,l1_1:addr] (ptr_v_2(char, l1, l1_1) | ptr l1), "&optarg")
praxi addback_c2ats_optarg {l1,l1_1:addr} (ptr_v_2(char, l1, l1_1) | ptr l1): void
macdef takeout_c2ats_optind = $extval([l1:addr] (ptr_v_1(int, l1) | ptr l1), "&optind")
praxi addback_c2ats_optind {l1:addr} (ptr_v_1(int, l1) | ptr l1): void
macdef takeout_c2ats_opterr = $extval([l1:addr] (ptr_v_1(int, l1) | ptr l1), "&opterr")
praxi addback_c2ats_opterr {l1:addr} (ptr_v_1(int, l1) | ptr l1): void
macdef takeout_c2ats_optopt = $extval([l1:addr] (ptr_v_1(int, l1) | ptr l1), "&optopt")
praxi addback_c2ats_optopt {l1:addr} (ptr_v_1(int, l1) | ptr l1): void
fun fun_c2ats_getopt: {l1,l1_1,l2:addr} (!ptr_v_2(char, l1, l1_1), !ptr_v_1(char, l2) | int, ptr l1, ptr l2) -> int = "mac#getopt"
%{#
#ifndef _STRUCT_C2ATS_ACCESSOR_H_
#define _STRUCT_C2ATS_ACCESSOR_H_
static inline struct _IO_marker* take_struct_c2ats__IO_marker__next(struct _IO_marker *p) { return (struct _IO_marker*) p->_next; }
static inline struct _IO_FILE* take_struct_c2ats__IO_marker__sbuf(struct _IO_marker *p) { return (struct _IO_FILE*) p->_sbuf; }
static inline char* take_struct_c2ats__IO_FILE__IO_read_ptr(struct _IO_FILE *p) { return (char*) p->_IO_read_ptr; }
static inline char* take_struct_c2ats__IO_FILE__IO_read_end(struct _IO_FILE *p) { return (char*) p->_IO_read_end; }
static inline char* take_struct_c2ats__IO_FILE__IO_read_base(struct _IO_FILE *p) { return (char*) p->_IO_read_base; }
static inline char* take_struct_c2ats__IO_FILE__IO_write_base(struct _IO_FILE *p) { return (char*) p->_IO_write_base; }
static inline char* take_struct_c2ats__IO_FILE__IO_write_ptr(struct _IO_FILE *p) { return (char*) p->_IO_write_ptr; }
static inline char* take_struct_c2ats__IO_FILE__IO_write_end(struct _IO_FILE *p) { return (char*) p->_IO_write_end; }
static inline char* take_struct_c2ats__IO_FILE__IO_buf_base(struct _IO_FILE *p) { return (char*) p->_IO_buf_base; }
static inline char* take_struct_c2ats__IO_FILE__IO_buf_end(struct _IO_FILE *p) { return (char*) p->_IO_buf_end; }
static inline char* take_struct_c2ats__IO_FILE__IO_save_base(struct _IO_FILE *p) { return (char*) p->_IO_save_base; }
static inline char* take_struct_c2ats__IO_FILE__IO_backup_base(struct _IO_FILE *p) { return (char*) p->_IO_backup_base; }
static inline char* take_struct_c2ats__IO_FILE__IO_save_end(struct _IO_FILE *p) { return (char*) p->_IO_save_end; }
static inline struct _IO_marker* take_struct_c2ats__IO_FILE__markers(struct _IO_FILE *p) { return (struct _IO_marker*) p->_markers; }
static inline struct _IO_FILE* take_struct_c2ats__IO_FILE__chain(struct _IO_FILE *p) { return (struct _IO_FILE*) p->_chain; }
static inline _IO_lock_t* take_struct_c2ats__IO_FILE__lock(struct _IO_FILE *p) { return (_IO_lock_t*) p->_lock; }
#endif /* _STRUCT_C2ATS_ACCESSOR_H_ */
%}
fun take_struct_c2ats__IO_marker__next: {l1:agz} (!ptr_v_1(struct_c2ats__IO_marker, l1) | ptr l1) -> [l2:addr] (ptr_v_1(struct_c2ats__IO_marker, l2), ptr_v_1(struct_c2ats__IO_marker, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_marker__sbuf: {l1:agz} (!ptr_v_1(struct_c2ats__IO_marker, l1) | ptr l1) -> [l2:addr] (ptr_v_1(struct_c2ats__IO_FILE, l2), ptr_v_1(struct_c2ats__IO_FILE, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_read_ptr: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_read_end: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_read_base: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_write_base: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_write_ptr: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_write_end: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_buf_base: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_buf_end: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_save_base: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_backup_base: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__IO_save_end: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(char, l2), ptr_v_1(char, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__markers: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(struct_c2ats__IO_marker, l2), ptr_v_1(struct_c2ats__IO_marker, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__chain: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(struct_c2ats__IO_FILE, l2), ptr_v_1(struct_c2ats__IO_FILE, l2) -<lin,prf> void | ptr l2) = "mac#"
fun take_struct_c2ats__IO_FILE__lock: {l1:agz} (!ptr_v_1(struct_c2ats__IO_FILE, l1) | ptr l1) -> [l2:addr] (ptr_v_1(type_c2ats__IO_lock_t, l2), ptr_v_1(type_c2ats__IO_lock_t, l2) -<lin,prf> void | ptr l2) = "mac#"

