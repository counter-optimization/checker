# 1 "Hacl_Bignum25519_51.h"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 369 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "Hacl_Bignum25519_51.h" 2
# 32 "Hacl_Bignum25519_51.h"
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 1 3 4
# 61 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/_types.h" 1 3 4
# 27 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/_types.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types.h" 1 3 4
# 32 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/cdefs.h" 1 3 4
# 649 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/cdefs.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_symbol_aliasing.h" 1 3 4
# 650 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/cdefs.h" 2 3 4
# 715 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/cdefs.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_posix_availability.h" 1 3 4
# 716 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/cdefs.h" 2 3 4
# 33 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/machine/_types.h" 1 3 4
# 32 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/machine/_types.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/_types.h" 1 3 4
# 37 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/_types.h" 3 4
typedef signed char __int8_t;



typedef unsigned char __uint8_t;
typedef short __int16_t;
typedef unsigned short __uint16_t;
typedef int __int32_t;
typedef unsigned int __uint32_t;
typedef long long __int64_t;
typedef unsigned long long __uint64_t;

typedef long __darwin_intptr_t;
typedef unsigned int __darwin_natural_t;
# 70 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/_types.h" 3 4
typedef int __darwin_ct_rune_t;





typedef union {
 char __mbstate8[128];
 long long _mbstateL;
} __mbstate_t;

typedef __mbstate_t __darwin_mbstate_t;


typedef long int __darwin_ptrdiff_t;







typedef long unsigned int __darwin_size_t;





typedef __builtin_va_list __darwin_va_list;





typedef int __darwin_wchar_t;




typedef __darwin_wchar_t __darwin_rune_t;


typedef int __darwin_wint_t;




typedef unsigned long __darwin_clock_t;
typedef __uint32_t __darwin_socklen_t;
typedef long __darwin_ssize_t;
typedef long __darwin_time_t;
# 33 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/machine/_types.h" 2 3 4
# 34 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types.h" 2 3 4
# 55 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types.h" 3 4
typedef __int64_t __darwin_blkcnt_t;
typedef __int32_t __darwin_blksize_t;
typedef __int32_t __darwin_dev_t;
typedef unsigned int __darwin_fsblkcnt_t;
typedef unsigned int __darwin_fsfilcnt_t;
typedef __uint32_t __darwin_gid_t;
typedef __uint32_t __darwin_id_t;
typedef __uint64_t __darwin_ino64_t;

typedef __darwin_ino64_t __darwin_ino_t;



typedef __darwin_natural_t __darwin_mach_port_name_t;
typedef __darwin_mach_port_name_t __darwin_mach_port_t;
typedef __uint16_t __darwin_mode_t;
typedef __int64_t __darwin_off_t;
typedef __int32_t __darwin_pid_t;
typedef __uint32_t __darwin_sigset_t;
typedef __int32_t __darwin_suseconds_t;
typedef __uint32_t __darwin_uid_t;
typedef __uint32_t __darwin_useconds_t;
typedef unsigned char __darwin_uuid_t[16];
typedef char __darwin_uuid_string_t[37];

# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_pthread/_pthread_types.h" 1 3 4
# 57 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_pthread/_pthread_types.h" 3 4
struct __darwin_pthread_handler_rec {
 void (*__routine)(void *);
 void *__arg;
 struct __darwin_pthread_handler_rec *__next;
};

struct _opaque_pthread_attr_t {
 long __sig;
 char __opaque[56];
};

struct _opaque_pthread_cond_t {
 long __sig;
 char __opaque[40];
};

struct _opaque_pthread_condattr_t {
 long __sig;
 char __opaque[8];
};

struct _opaque_pthread_mutex_t {
 long __sig;
 char __opaque[56];
};

struct _opaque_pthread_mutexattr_t {
 long __sig;
 char __opaque[8];
};

struct _opaque_pthread_once_t {
 long __sig;
 char __opaque[8];
};

struct _opaque_pthread_rwlock_t {
 long __sig;
 char __opaque[192];
};

struct _opaque_pthread_rwlockattr_t {
 long __sig;
 char __opaque[16];
};

struct _opaque_pthread_t {
 long __sig;
 struct __darwin_pthread_handler_rec *__cleanup_stack;
 char __opaque[8176];
};

typedef struct _opaque_pthread_attr_t __darwin_pthread_attr_t;
typedef struct _opaque_pthread_cond_t __darwin_pthread_cond_t;
typedef struct _opaque_pthread_condattr_t __darwin_pthread_condattr_t;
typedef unsigned long __darwin_pthread_key_t;
typedef struct _opaque_pthread_mutex_t __darwin_pthread_mutex_t;
typedef struct _opaque_pthread_mutexattr_t __darwin_pthread_mutexattr_t;
typedef struct _opaque_pthread_once_t __darwin_pthread_once_t;
typedef struct _opaque_pthread_rwlock_t __darwin_pthread_rwlock_t;
typedef struct _opaque_pthread_rwlockattr_t __darwin_pthread_rwlockattr_t;
typedef struct _opaque_pthread_t *__darwin_pthread_t;
# 81 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types.h" 2 3 4
# 28 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/_types.h" 2 3 4
# 40 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/_types.h" 3 4
typedef int __darwin_nl_item;
typedef int __darwin_wctrans_t;

typedef __uint32_t __darwin_wctype_t;
# 62 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 2 3 4

# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/Availability.h" 1 3 4
# 135 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/Availability.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/AvailabilityVersions.h" 1 3 4
# 136 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/Availability.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/AvailabilityInternal.h" 1 3 4
# 137 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/Availability.h" 2 3 4
# 64 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_size_t.h" 1 3 4
# 31 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_size_t.h" 3 4
typedef __darwin_size_t size_t;
# 65 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_null.h" 1 3 4
# 66 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 2 3 4




void *memchr(const void *__s, int __c, size_t __n);
int memcmp(const void *__s1, const void *__s2, size_t __n);
void *memcpy(void *__dst, const void *__src, size_t __n);
void *memmove(void *__dst, const void *__src, size_t __len);
void *memset(void *__b, int __c, size_t __len);
char *strcat(char *__s1, const char *__s2);
char *strchr(const char *__s, int __c);
int strcmp(const char *__s1, const char *__s2);
int strcoll(const char *__s1, const char *__s2);
char *strcpy(char *__dst, const char *__src);
size_t strcspn(const char *__s, const char *__charset);
char *strerror(int __errnum) __asm("_" "strerror" );
size_t strlen(const char *__s);
char *strncat(char *__s1, const char *__s2, size_t __n);
int strncmp(const char *__s1, const char *__s2, size_t __n);
char *strncpy(char *__dst, const char *__src, size_t __n);
char *strpbrk(const char *__s, const char *__charset);
char *strrchr(const char *__s, int __c);
size_t strspn(const char *__s, const char *__charset);
char *strstr(const char *__big, const char *__little);
char *strtok(char *__str, const char *__sep);
size_t strxfrm(char *__s1, const char *__s2, size_t __n);
# 104 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 3 4
char *strtok_r(char *__str, const char *__sep, char **__lasts);
# 116 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 3 4
int strerror_r(int __errnum, char *__strerrbuf, size_t __buflen);
char *strdup(const char *__s1);
void *memccpy(void *__dst, const void *__src, int __c, size_t __n);
# 130 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 3 4
char *stpcpy(char *__dst, const char *__src);
char *stpncpy(char *__dst, const char *__src, size_t __n) __attribute__((availability(macosx,introduced=10.7)));
char *strndup(const char *__s1, size_t __n) __attribute__((availability(macosx,introduced=10.7)));
size_t strnlen(const char *__s1, size_t __n) __attribute__((availability(macosx,introduced=10.7)));
char *strsignal(int __sig);






# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_rsize_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_rsize_t.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/machine/types.h" 1 3 4
# 35 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/machine/types.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 1 3 4
# 76 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_int8_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_int8_t.h" 3 4
typedef signed char int8_t;
# 77 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_int16_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_int16_t.h" 3 4
typedef short int16_t;
# 78 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_int32_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_int32_t.h" 3 4
typedef int int32_t;
# 79 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_int64_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_int64_t.h" 3 4
typedef long long int64_t;
# 80 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 2 3 4

# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_u_int8_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_u_int8_t.h" 3 4
typedef unsigned char u_int8_t;
# 82 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_u_int16_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_u_int16_t.h" 3 4
typedef unsigned short u_int16_t;
# 83 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_u_int32_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_u_int32_t.h" 3 4
typedef unsigned int u_int32_t;
# 84 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_u_int64_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_u_int64_t.h" 3 4
typedef unsigned long long u_int64_t;
# 85 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 2 3 4


typedef int64_t register_t;




# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_intptr_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_intptr_t.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/machine/types.h" 1 3 4
# 31 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_intptr_t.h" 2 3 4

typedef __darwin_intptr_t intptr_t;
# 93 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_uintptr_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_uintptr_t.h" 3 4
typedef unsigned long uintptr_t;
# 94 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/i386/types.h" 2 3 4



typedef u_int64_t user_addr_t;
typedef u_int64_t user_size_t;
typedef int64_t user_ssize_t;
typedef int64_t user_long_t;
typedef u_int64_t user_ulong_t;
typedef int64_t user_time_t;
typedef int64_t user_off_t;







typedef u_int64_t syscall_arg_t;
# 36 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/machine/types.h" 2 3 4
# 31 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_rsize_t.h" 2 3 4
typedef __darwin_size_t rsize_t;
# 142 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 2 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_errno_t.h" 1 3 4
# 30 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_errno_t.h" 3 4
typedef int errno_t;
# 143 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 2 3 4


errno_t memset_s(void *__s, rsize_t __smax, int __c, rsize_t __n) __attribute__((availability(macosx,introduced=10.9)));






# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_ssize_t.h" 1 3 4
# 31 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/sys/_types/_ssize_t.h" 3 4
typedef __darwin_ssize_t ssize_t;
# 153 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 2 3 4


void *memmem(const void *__big, size_t __big_len, const void *__little, size_t __little_len) __attribute__((availability(macosx,introduced=10.7)));
void memset_pattern4(void *__b, const void *__pattern4, size_t __len) __attribute__((availability(macosx,introduced=10.5)));
void memset_pattern8(void *__b, const void *__pattern8, size_t __len) __attribute__((availability(macosx,introduced=10.5)));
void memset_pattern16(void *__b, const void *__pattern16, size_t __len) __attribute__((availability(macosx,introduced=10.5)));

char *strcasestr(const char *__big, const char *__little);
char *strnstr(const char *__big, const char *__little, size_t __len);
size_t strlcat(char *__dst, const char *__source, size_t __size);
size_t strlcpy(char *__dst, const char *__source, size_t __size);
void strmode(int __mode, char *__bp);
char *strsep(char **__stringp, const char *__delim);


void swab(const void * restrict, void * restrict, ssize_t);

__attribute__((availability(macosx,introduced=10.12.1))) __attribute__((availability(ios,introduced=10.1)))
__attribute__((availability(tvos,introduced=10.0.1))) __attribute__((availability(watchos,introduced=3.1)))
int timingsafe_bcmp(const void *__b1, const void *__b2, size_t __len);

__attribute__((availability(macosx,introduced=11.0))) __attribute__((availability(ios,introduced=14.0)))
__attribute__((availability(tvos,introduced=14.0))) __attribute__((availability(watchos,introduced=7.0)))
int strsignal_r(int __sig, char *__strsignalbuf, size_t __buflen);







# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/strings.h" 1 3 4
# 70 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/strings.h" 3 4
int bcmp(const void *, const void *, size_t) ;
void bcopy(const void *, void *, size_t) ;
void bzero(void *, size_t) ;
char *index(const char *, int) ;
char *rindex(const char *, int) ;


int ffs(int);
int strcasecmp(const char *, const char *);
int strncasecmp(const char *, const char *, size_t);





int ffsl(long) __attribute__((availability(macosx,introduced=10.5)));
int ffsll(long long) __attribute__((availability(macosx,introduced=10.9)));
int fls(int) __attribute__((availability(macosx,introduced=10.5)));
int flsl(long) __attribute__((availability(macosx,introduced=10.5)));
int flsll(long long) __attribute__((availability(macosx,introduced=10.9)));


# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 1 3 4
# 93 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/strings.h" 2 3 4




# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/secure/_strings.h" 1 3 4
# 33 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/secure/_strings.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/secure/_common.h" 1 3 4
# 34 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/secure/_strings.h" 2 3 4
# 98 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/strings.h" 2 3 4
# 185 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 2 3 4
# 194 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 3 4
# 1 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/secure/_string.h" 1 3 4
# 195 "/Library/Developer/CommandLineTools/SDKs/MacOSX11.sdk/usr/include/string.h" 2 3 4
# 33 "Hacl_Bignum25519_51.h" 2







static inline void Hacl_Impl_Curve25519_Field51_fadd(u64 *out, u64 *f1, u64 *f2)
{
  u64 f10 = f1[0U];
  u64 f20 = f2[0U];
  u64 f11 = f1[1U];
  u64 f21 = f2[1U];
  u64 f12 = f1[2U];
  u64 f22 = f2[2U];
  u64 f13 = f1[3U];
  u64 f23 = f2[3U];
  u64 f14 = f1[4U];
  u64 f24 = f2[4U];
  out[0U] = f10 + f20;
  out[1U] = f11 + f21;
  out[2U] = f12 + f22;
  out[3U] = f13 + f23;
  out[4U] = f14 + f24;
}

static inline void Hacl_Impl_Curve25519_Field51_fsub(u64 *out, u64 *f1, u64 *f2)
{
  u64 f10 = f1[0U];
  u64 f20 = f2[0U];
  u64 f11 = f1[1U];
  u64 f21 = f2[1U];
  u64 f12 = f1[2U];
  u64 f22 = f2[2U];
  u64 f13 = f1[3U];
  u64 f23 = f2[3U];
  u64 f14 = f1[4U];
  u64 f24 = f2[4U];
  out[0U] = f10 + (u64)0x3fffffffffff68U - f20;
  out[1U] = f11 + (u64)0x3ffffffffffff8U - f21;
  out[2U] = f12 + (u64)0x3ffffffffffff8U - f22;
  out[3U] = f13 + (u64)0x3ffffffffffff8U - f23;
  out[4U] = f14 + (u64)0x3ffffffffffff8U - f24;
}

static inline void
Hacl_Impl_Curve25519_Field51_fmul(u64 *out, u64 *f1, u64 *f2, uint128_t *uu___)
{
  u64 f10 = f1[0U];
  u64 f11 = f1[1U];
  u64 f12 = f1[2U];
  u64 f13 = f1[3U];
  u64 f14 = f1[4U];
  u64 f20 = f2[0U];
  u64 f21 = f2[1U];
  u64 f22 = f2[2U];
  u64 f23 = f2[3U];
  u64 f24 = f2[4U];
  u64 tmp1 = f21 * (u64)19U;
  u64 tmp2 = f22 * (u64)19U;
  u64 tmp3 = f23 * (u64)19U;
  u64 tmp4 = f24 * (u64)19U;
  uint128_t o00 = (uint128_t)f10 * f20;
  uint128_t o10 = (uint128_t)f10 * f21;
  uint128_t o20 = (uint128_t)f10 * f22;
  uint128_t o30 = (uint128_t)f10 * f23;
  uint128_t o40 = (uint128_t)f10 * f24;
  uint128_t o01 = o00 + (uint128_t)f11 * tmp4;
  uint128_t o11 = o10 + (uint128_t)f11 * f20;
  uint128_t o21 = o20 + (uint128_t)f11 * f21;
  uint128_t o31 = o30 + (uint128_t)f11 * f22;
  uint128_t o41 = o40 + (uint128_t)f11 * f23;
  uint128_t o02 = o01 + (uint128_t)f12 * tmp3;
  uint128_t o12 = o11 + (uint128_t)f12 * tmp4;
  uint128_t o22 = o21 + (uint128_t)f12 * f20;
  uint128_t o32 = o31 + (uint128_t)f12 * f21;
  uint128_t o42 = o41 + (uint128_t)f12 * f22;
  uint128_t o03 = o02 + (uint128_t)f13 * tmp2;
  uint128_t o13 = o12 + (uint128_t)f13 * tmp3;
  uint128_t o23 = o22 + (uint128_t)f13 * tmp4;
  uint128_t o33 = o32 + (uint128_t)f13 * f20;
  uint128_t o43 = o42 + (uint128_t)f13 * f21;
  uint128_t o04 = o03 + (uint128_t)f14 * tmp1;
  uint128_t o14 = o13 + (uint128_t)f14 * tmp2;
  uint128_t o24 = o23 + (uint128_t)f14 * tmp3;
  uint128_t o34 = o33 + (uint128_t)f14 * tmp4;
  uint128_t o44 = o43 + (uint128_t)f14 * f20;
  uint128_t tmp_w0 = o04;
  uint128_t tmp_w1 = o14;
  uint128_t tmp_w2 = o24;
  uint128_t tmp_w3 = o34;
  uint128_t tmp_w4 = o44;
  uint128_t l_ = tmp_w0 + (uint128_t)(u64)0U;
  u64 tmp01 = (uint64_t)l_ & (u64)0x7ffffffffffffU;
  u64 c0 = (uint64_t)(l_ >> (u32)51U);
  uint128_t l_0 = tmp_w1 + (uint128_t)c0;
  u64 tmp11 = (uint64_t)l_0 & (u64)0x7ffffffffffffU;
  u64 c1 = (uint64_t)(l_0 >> (u32)51U);
  uint128_t l_1 = tmp_w2 + (uint128_t)c1;
  u64 tmp21 = (uint64_t)l_1 & (u64)0x7ffffffffffffU;
  u64 c2 = (uint64_t)(l_1 >> (u32)51U);
  uint128_t l_2 = tmp_w3 + (uint128_t)c2;
  u64 tmp31 = (uint64_t)l_2 & (u64)0x7ffffffffffffU;
  u64 c3 = (uint64_t)(l_2 >> (u32)51U);
  uint128_t l_3 = tmp_w4 + (uint128_t)c3;
  u64 tmp41 = (uint64_t)l_3 & (u64)0x7ffffffffffffU;
  u64 c4 = (uint64_t)(l_3 >> (u32)51U);
  u64 l_4 = tmp01 + c4 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c5 = l_4 >> (u32)51U;
  u64 o0 = tmp0_;
  u64 o1 = tmp11 + c5;
  u64 o2 = tmp21;
  u64 o3 = tmp31;
  u64 o4 = tmp41;
  out[0U] = o0;
  out[1U] = o1;
  out[2U] = o2;
  out[3U] = o3;
  out[4U] = o4;
}

static inline void
Hacl_Impl_Curve25519_Field51_fmul2(u64 *out, u64 *f1, u64 *f2, uint128_t *uu___)
{
  u64 f10 = f1[0U];
  u64 f11 = f1[1U];
  u64 f12 = f1[2U];
  u64 f13 = f1[3U];
  u64 f14 = f1[4U];
  u64 f20 = f2[0U];
  u64 f21 = f2[1U];
  u64 f22 = f2[2U];
  u64 f23 = f2[3U];
  u64 f24 = f2[4U];
  u64 f30 = f1[5U];
  u64 f31 = f1[6U];
  u64 f32 = f1[7U];
  u64 f33 = f1[8U];
  u64 f34 = f1[9U];
  u64 f40 = f2[5U];
  u64 f41 = f2[6U];
  u64 f42 = f2[7U];
  u64 f43 = f2[8U];
  u64 f44 = f2[9U];
  u64 tmp11 = f21 * (u64)19U;
  u64 tmp12 = f22 * (u64)19U;
  u64 tmp13 = f23 * (u64)19U;
  u64 tmp14 = f24 * (u64)19U;
  u64 tmp21 = f41 * (u64)19U;
  u64 tmp22 = f42 * (u64)19U;
  u64 tmp23 = f43 * (u64)19U;
  u64 tmp24 = f44 * (u64)19U;
  uint128_t o00 = (uint128_t)f10 * f20;
  uint128_t o15 = (uint128_t)f10 * f21;
  uint128_t o25 = (uint128_t)f10 * f22;
  uint128_t o30 = (uint128_t)f10 * f23;
  uint128_t o40 = (uint128_t)f10 * f24;
  uint128_t o010 = o00 + (uint128_t)f11 * tmp14;
  uint128_t o110 = o15 + (uint128_t)f11 * f20;
  uint128_t o210 = o25 + (uint128_t)f11 * f21;
  uint128_t o310 = o30 + (uint128_t)f11 * f22;
  uint128_t o410 = o40 + (uint128_t)f11 * f23;
  uint128_t o020 = o010 + (uint128_t)f12 * tmp13;
  uint128_t o120 = o110 + (uint128_t)f12 * tmp14;
  uint128_t o220 = o210 + (uint128_t)f12 * f20;
  uint128_t o320 = o310 + (uint128_t)f12 * f21;
  uint128_t o420 = o410 + (uint128_t)f12 * f22;
  uint128_t o030 = o020 + (uint128_t)f13 * tmp12;
  uint128_t o130 = o120 + (uint128_t)f13 * tmp13;
  uint128_t o230 = o220 + (uint128_t)f13 * tmp14;
  uint128_t o330 = o320 + (uint128_t)f13 * f20;
  uint128_t o430 = o420 + (uint128_t)f13 * f21;
  uint128_t o040 = o030 + (uint128_t)f14 * tmp11;
  uint128_t o140 = o130 + (uint128_t)f14 * tmp12;
  uint128_t o240 = o230 + (uint128_t)f14 * tmp13;
  uint128_t o340 = o330 + (uint128_t)f14 * tmp14;
  uint128_t o440 = o430 + (uint128_t)f14 * f20;
  uint128_t tmp_w10 = o040;
  uint128_t tmp_w11 = o140;
  uint128_t tmp_w12 = o240;
  uint128_t tmp_w13 = o340;
  uint128_t tmp_w14 = o440;
  uint128_t o0 = (uint128_t)f30 * f40;
  uint128_t o1 = (uint128_t)f30 * f41;
  uint128_t o2 = (uint128_t)f30 * f42;
  uint128_t o3 = (uint128_t)f30 * f43;
  uint128_t o4 = (uint128_t)f30 * f44;
  uint128_t o01 = o0 + (uint128_t)f31 * tmp24;
  uint128_t o111 = o1 + (uint128_t)f31 * f40;
  uint128_t o211 = o2 + (uint128_t)f31 * f41;
  uint128_t o31 = o3 + (uint128_t)f31 * f42;
  uint128_t o41 = o4 + (uint128_t)f31 * f43;
  uint128_t o02 = o01 + (uint128_t)f32 * tmp23;
  uint128_t o121 = o111 + (uint128_t)f32 * tmp24;
  uint128_t o221 = o211 + (uint128_t)f32 * f40;
  uint128_t o32 = o31 + (uint128_t)f32 * f41;
  uint128_t o42 = o41 + (uint128_t)f32 * f42;
  uint128_t o03 = o02 + (uint128_t)f33 * tmp22;
  uint128_t o131 = o121 + (uint128_t)f33 * tmp23;
  uint128_t o231 = o221 + (uint128_t)f33 * tmp24;
  uint128_t o33 = o32 + (uint128_t)f33 * f40;
  uint128_t o43 = o42 + (uint128_t)f33 * f41;
  uint128_t o04 = o03 + (uint128_t)f34 * tmp21;
  uint128_t o141 = o131 + (uint128_t)f34 * tmp22;
  uint128_t o241 = o231 + (uint128_t)f34 * tmp23;
  uint128_t o34 = o33 + (uint128_t)f34 * tmp24;
  uint128_t o44 = o43 + (uint128_t)f34 * f40;
  uint128_t tmp_w20 = o04;
  uint128_t tmp_w21 = o141;
  uint128_t tmp_w22 = o241;
  uint128_t tmp_w23 = o34;
  uint128_t tmp_w24 = o44;
  uint128_t l_ = tmp_w10 + (uint128_t)(u64)0U;
  u64 tmp00 = (uint64_t)l_ & (u64)0x7ffffffffffffU;
  u64 c00 = (uint64_t)(l_ >> (u32)51U);
  uint128_t l_0 = tmp_w11 + (uint128_t)c00;
  u64 tmp10 = (uint64_t)l_0 & (u64)0x7ffffffffffffU;
  u64 c10 = (uint64_t)(l_0 >> (u32)51U);
  uint128_t l_1 = tmp_w12 + (uint128_t)c10;
  u64 tmp20 = (uint64_t)l_1 & (u64)0x7ffffffffffffU;
  u64 c20 = (uint64_t)(l_1 >> (u32)51U);
  uint128_t l_2 = tmp_w13 + (uint128_t)c20;
  u64 tmp30 = (uint64_t)l_2 & (u64)0x7ffffffffffffU;
  u64 c30 = (uint64_t)(l_2 >> (u32)51U);
  uint128_t l_3 = tmp_w14 + (uint128_t)c30;
  u64 tmp40 = (uint64_t)l_3 & (u64)0x7ffffffffffffU;
  u64 c40 = (uint64_t)(l_3 >> (u32)51U);
  u64 l_4 = tmp00 + c40 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c50 = l_4 >> (u32)51U;
  u64 o100 = tmp0_;
  u64 o112 = tmp10 + c50;
  u64 o122 = tmp20;
  u64 o132 = tmp30;
  u64 o142 = tmp40;
  uint128_t l_5 = tmp_w20 + (uint128_t)(u64)0U;
  u64 tmp0 = (uint64_t)l_5 & (u64)0x7ffffffffffffU;
  u64 c0 = (uint64_t)(l_5 >> (u32)51U);
  uint128_t l_6 = tmp_w21 + (uint128_t)c0;
  u64 tmp1 = (uint64_t)l_6 & (u64)0x7ffffffffffffU;
  u64 c1 = (uint64_t)(l_6 >> (u32)51U);
  uint128_t l_7 = tmp_w22 + (uint128_t)c1;
  u64 tmp2 = (uint64_t)l_7 & (u64)0x7ffffffffffffU;
  u64 c2 = (uint64_t)(l_7 >> (u32)51U);
  uint128_t l_8 = tmp_w23 + (uint128_t)c2;
  u64 tmp3 = (uint64_t)l_8 & (u64)0x7ffffffffffffU;
  u64 c3 = (uint64_t)(l_8 >> (u32)51U);
  uint128_t l_9 = tmp_w24 + (uint128_t)c3;
  u64 tmp4 = (uint64_t)l_9 & (u64)0x7ffffffffffffU;
  u64 c4 = (uint64_t)(l_9 >> (u32)51U);
  u64 l_10 = tmp0 + c4 * (u64)19U;
  u64 tmp0_0 = l_10 & (u64)0x7ffffffffffffU;
  u64 c5 = l_10 >> (u32)51U;
  u64 o200 = tmp0_0;
  u64 o212 = tmp1 + c5;
  u64 o222 = tmp2;
  u64 o232 = tmp3;
  u64 o242 = tmp4;
  u64 o10 = o100;
  u64 o11 = o112;
  u64 o12 = o122;
  u64 o13 = o132;
  u64 o14 = o142;
  u64 o20 = o200;
  u64 o21 = o212;
  u64 o22 = o222;
  u64 o23 = o232;
  u64 o24 = o242;
  out[0U] = o10;
  out[1U] = o11;
  out[2U] = o12;
  out[3U] = o13;
  out[4U] = o14;
  out[5U] = o20;
  out[6U] = o21;
  out[7U] = o22;
  out[8U] = o23;
  out[9U] = o24;
}

static inline void Hacl_Impl_Curve25519_Field51_fmul1(u64 *out, u64 *f1, u64 f2)
{
  u64 f10 = f1[0U];
  u64 f11 = f1[1U];
  u64 f12 = f1[2U];
  u64 f13 = f1[3U];
  u64 f14 = f1[4U];
  uint128_t tmp_w0 = (uint128_t)f2 * f10;
  uint128_t tmp_w1 = (uint128_t)f2 * f11;
  uint128_t tmp_w2 = (uint128_t)f2 * f12;
  uint128_t tmp_w3 = (uint128_t)f2 * f13;
  uint128_t tmp_w4 = (uint128_t)f2 * f14;
  uint128_t l_ = tmp_w0 + (uint128_t)(u64)0U;
  u64 tmp0 = (uint64_t)l_ & (u64)0x7ffffffffffffU;
  u64 c0 = (uint64_t)(l_ >> (u32)51U);
  uint128_t l_0 = tmp_w1 + (uint128_t)c0;
  u64 tmp1 = (uint64_t)l_0 & (u64)0x7ffffffffffffU;
  u64 c1 = (uint64_t)(l_0 >> (u32)51U);
  uint128_t l_1 = tmp_w2 + (uint128_t)c1;
  u64 tmp2 = (uint64_t)l_1 & (u64)0x7ffffffffffffU;
  u64 c2 = (uint64_t)(l_1 >> (u32)51U);
  uint128_t l_2 = tmp_w3 + (uint128_t)c2;
  u64 tmp3 = (uint64_t)l_2 & (u64)0x7ffffffffffffU;
  u64 c3 = (uint64_t)(l_2 >> (u32)51U);
  uint128_t l_3 = tmp_w4 + (uint128_t)c3;
  u64 tmp4 = (uint64_t)l_3 & (u64)0x7ffffffffffffU;
  u64 c4 = (uint64_t)(l_3 >> (u32)51U);
  u64 l_4 = tmp0 + c4 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c5 = l_4 >> (u32)51U;
  u64 o0 = tmp0_;
  u64 o1 = tmp1 + c5;
  u64 o2 = tmp2;
  u64 o3 = tmp3;
  u64 o4 = tmp4;
  out[0U] = o0;
  out[1U] = o1;
  out[2U] = o2;
  out[3U] = o3;
  out[4U] = o4;
}

static inline void Hacl_Impl_Curve25519_Field51_fsqr(u64 *out, u64 *f, uint128_t *uu___)
{
  u64 f0 = f[0U];
  u64 f1 = f[1U];
  u64 f2 = f[2U];
  u64 f3 = f[3U];
  u64 f4 = f[4U];
  u64 d0 = (u64)2U * f0;
  u64 d1 = (u64)2U * f1;
  u64 d2 = (u64)38U * f2;
  u64 d3 = (u64)19U * f3;
  u64 d419 = (u64)19U * f4;
  u64 d4 = (u64)2U * d419;
  uint128_t s0 = (uint128_t)f0 * f0 + (uint128_t)d4 * f1 + (uint128_t)d2 * f3;
  uint128_t s1 = (uint128_t)d0 * f1 + (uint128_t)d4 * f2 + (uint128_t)d3 * f3;
  uint128_t s2 = (uint128_t)d0 * f2 + (uint128_t)f1 * f1 + (uint128_t)d4 * f3;
  uint128_t s3 = (uint128_t)d0 * f3 + (uint128_t)d1 * f2 + (uint128_t)f4 * d419;
  uint128_t s4 = (uint128_t)d0 * f4 + (uint128_t)d1 * f3 + (uint128_t)f2 * f2;
  uint128_t o00 = s0;
  uint128_t o10 = s1;
  uint128_t o20 = s2;
  uint128_t o30 = s3;
  uint128_t o40 = s4;
  uint128_t l_ = o00 + (uint128_t)(u64)0U;
  u64 tmp0 = (uint64_t)l_ & (u64)0x7ffffffffffffU;
  u64 c0 = (uint64_t)(l_ >> (u32)51U);
  uint128_t l_0 = o10 + (uint128_t)c0;
  u64 tmp1 = (uint64_t)l_0 & (u64)0x7ffffffffffffU;
  u64 c1 = (uint64_t)(l_0 >> (u32)51U);
  uint128_t l_1 = o20 + (uint128_t)c1;
  u64 tmp2 = (uint64_t)l_1 & (u64)0x7ffffffffffffU;
  u64 c2 = (uint64_t)(l_1 >> (u32)51U);
  uint128_t l_2 = o30 + (uint128_t)c2;
  u64 tmp3 = (uint64_t)l_2 & (u64)0x7ffffffffffffU;
  u64 c3 = (uint64_t)(l_2 >> (u32)51U);
  uint128_t l_3 = o40 + (uint128_t)c3;
  u64 tmp4 = (uint64_t)l_3 & (u64)0x7ffffffffffffU;
  u64 c4 = (uint64_t)(l_3 >> (u32)51U);
  u64 l_4 = tmp0 + c4 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c5 = l_4 >> (u32)51U;
  u64 o0 = tmp0_;
  u64 o1 = tmp1 + c5;
  u64 o2 = tmp2;
  u64 o3 = tmp3;
  u64 o4 = tmp4;
  out[0U] = o0;
  out[1U] = o1;
  out[2U] = o2;
  out[3U] = o3;
  out[4U] = o4;
}

static inline void Hacl_Impl_Curve25519_Field51_fsqr2(u64 *out, u64 *f, uint128_t *uu___)
{
  u64 f10 = f[0U];
  u64 f11 = f[1U];
  u64 f12 = f[2U];
  u64 f13 = f[3U];
  u64 f14 = f[4U];
  u64 f20 = f[5U];
  u64 f21 = f[6U];
  u64 f22 = f[7U];
  u64 f23 = f[8U];
  u64 f24 = f[9U];
  u64 d00 = (u64)2U * f10;
  u64 d10 = (u64)2U * f11;
  u64 d20 = (u64)38U * f12;
  u64 d30 = (u64)19U * f13;
  u64 d4190 = (u64)19U * f14;
  u64 d40 = (u64)2U * d4190;
  uint128_t s00 = (uint128_t)f10 * f10 + (uint128_t)d40 * f11 + (uint128_t)d20 * f13;
  uint128_t s10 = (uint128_t)d00 * f11 + (uint128_t)d40 * f12 + (uint128_t)d30 * f13;
  uint128_t s20 = (uint128_t)d00 * f12 + (uint128_t)f11 * f11 + (uint128_t)d40 * f13;
  uint128_t s30 = (uint128_t)d00 * f13 + (uint128_t)d10 * f12 + (uint128_t)f14 * d4190;
  uint128_t s40 = (uint128_t)d00 * f14 + (uint128_t)d10 * f13 + (uint128_t)f12 * f12;
  uint128_t o100 = s00;
  uint128_t o110 = s10;
  uint128_t o120 = s20;
  uint128_t o130 = s30;
  uint128_t o140 = s40;
  u64 d0 = (u64)2U * f20;
  u64 d1 = (u64)2U * f21;
  u64 d2 = (u64)38U * f22;
  u64 d3 = (u64)19U * f23;
  u64 d419 = (u64)19U * f24;
  u64 d4 = (u64)2U * d419;
  uint128_t s0 = (uint128_t)f20 * f20 + (uint128_t)d4 * f21 + (uint128_t)d2 * f23;
  uint128_t s1 = (uint128_t)d0 * f21 + (uint128_t)d4 * f22 + (uint128_t)d3 * f23;
  uint128_t s2 = (uint128_t)d0 * f22 + (uint128_t)f21 * f21 + (uint128_t)d4 * f23;
  uint128_t s3 = (uint128_t)d0 * f23 + (uint128_t)d1 * f22 + (uint128_t)f24 * d419;
  uint128_t s4 = (uint128_t)d0 * f24 + (uint128_t)d1 * f23 + (uint128_t)f22 * f22;
  uint128_t o200 = s0;
  uint128_t o210 = s1;
  uint128_t o220 = s2;
  uint128_t o230 = s3;
  uint128_t o240 = s4;
  uint128_t l_ = o100 + (uint128_t)(u64)0U;
  u64 tmp00 = (uint64_t)l_ & (u64)0x7ffffffffffffU;
  u64 c00 = (uint64_t)(l_ >> (u32)51U);
  uint128_t l_0 = o110 + (uint128_t)c00;
  u64 tmp10 = (uint64_t)l_0 & (u64)0x7ffffffffffffU;
  u64 c10 = (uint64_t)(l_0 >> (u32)51U);
  uint128_t l_1 = o120 + (uint128_t)c10;
  u64 tmp20 = (uint64_t)l_1 & (u64)0x7ffffffffffffU;
  u64 c20 = (uint64_t)(l_1 >> (u32)51U);
  uint128_t l_2 = o130 + (uint128_t)c20;
  u64 tmp30 = (uint64_t)l_2 & (u64)0x7ffffffffffffU;
  u64 c30 = (uint64_t)(l_2 >> (u32)51U);
  uint128_t l_3 = o140 + (uint128_t)c30;
  u64 tmp40 = (uint64_t)l_3 & (u64)0x7ffffffffffffU;
  u64 c40 = (uint64_t)(l_3 >> (u32)51U);
  u64 l_4 = tmp00 + c40 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c50 = l_4 >> (u32)51U;
  u64 o101 = tmp0_;
  u64 o111 = tmp10 + c50;
  u64 o121 = tmp20;
  u64 o131 = tmp30;
  u64 o141 = tmp40;
  uint128_t l_5 = o200 + (uint128_t)(u64)0U;
  u64 tmp0 = (uint64_t)l_5 & (u64)0x7ffffffffffffU;
  u64 c0 = (uint64_t)(l_5 >> (u32)51U);
  uint128_t l_6 = o210 + (uint128_t)c0;
  u64 tmp1 = (uint64_t)l_6 & (u64)0x7ffffffffffffU;
  u64 c1 = (uint64_t)(l_6 >> (u32)51U);
  uint128_t l_7 = o220 + (uint128_t)c1;
  u64 tmp2 = (uint64_t)l_7 & (u64)0x7ffffffffffffU;
  u64 c2 = (uint64_t)(l_7 >> (u32)51U);
  uint128_t l_8 = o230 + (uint128_t)c2;
  u64 tmp3 = (uint64_t)l_8 & (u64)0x7ffffffffffffU;
  u64 c3 = (uint64_t)(l_8 >> (u32)51U);
  uint128_t l_9 = o240 + (uint128_t)c3;
  u64 tmp4 = (uint64_t)l_9 & (u64)0x7ffffffffffffU;
  u64 c4 = (uint64_t)(l_9 >> (u32)51U);
  u64 l_10 = tmp0 + c4 * (u64)19U;
  u64 tmp0_0 = l_10 & (u64)0x7ffffffffffffU;
  u64 c5 = l_10 >> (u32)51U;
  u64 o201 = tmp0_0;
  u64 o211 = tmp1 + c5;
  u64 o221 = tmp2;
  u64 o231 = tmp3;
  u64 o241 = tmp4;
  u64 o10 = o101;
  u64 o11 = o111;
  u64 o12 = o121;
  u64 o13 = o131;
  u64 o14 = o141;
  u64 o20 = o201;
  u64 o21 = o211;
  u64 o22 = o221;
  u64 o23 = o231;
  u64 o24 = o241;
  out[0U] = o10;
  out[1U] = o11;
  out[2U] = o12;
  out[3U] = o13;
  out[4U] = o14;
  out[5U] = o20;
  out[6U] = o21;
  out[7U] = o22;
  out[8U] = o23;
  out[9U] = o24;
}

static inline void Hacl_Impl_Curve25519_Field51_store_felem(u64 *u64s, u64 *f)
{
  u64 f0 = f[0U];
  u64 f1 = f[1U];
  u64 f2 = f[2U];
  u64 f3 = f[3U];
  u64 f4 = f[4U];
  u64 l_ = f0 + (u64)0U;
  u64 tmp0 = l_ & (u64)0x7ffffffffffffU;
  u64 c0 = l_ >> (u32)51U;
  u64 l_0 = f1 + c0;
  u64 tmp1 = l_0 & (u64)0x7ffffffffffffU;
  u64 c1 = l_0 >> (u32)51U;
  u64 l_1 = f2 + c1;
  u64 tmp2 = l_1 & (u64)0x7ffffffffffffU;
  u64 c2 = l_1 >> (u32)51U;
  u64 l_2 = f3 + c2;
  u64 tmp3 = l_2 & (u64)0x7ffffffffffffU;
  u64 c3 = l_2 >> (u32)51U;
  u64 l_3 = f4 + c3;
  u64 tmp4 = l_3 & (u64)0x7ffffffffffffU;
  u64 c4 = l_3 >> (u32)51U;
  u64 l_4 = tmp0 + c4 * (u64)19U;
  u64 tmp0_ = l_4 & (u64)0x7ffffffffffffU;
  u64 c5 = l_4 >> (u32)51U;
  u64 f01 = tmp0_;
  u64 f11 = tmp1 + c5;
  u64 f21 = tmp2;
  u64 f31 = tmp3;
  u64 f41 = tmp4;
  u64 m0 = FStar_UInt64_gte_mask(f01, (u64)0x7ffffffffffedU);
  u64 m1 = FStar_UInt64_eq_mask(f11, (u64)0x7ffffffffffffU);
  u64 m2 = FStar_UInt64_eq_mask(f21, (u64)0x7ffffffffffffU);
  u64 m3 = FStar_UInt64_eq_mask(f31, (u64)0x7ffffffffffffU);
  u64 m4 = FStar_UInt64_eq_mask(f41, (u64)0x7ffffffffffffU);
  u64 mask = (((m0 & m1) & m2) & m3) & m4;
  u64 f0_ = f01 - (mask & (u64)0x7ffffffffffedU);
  u64 f1_ = f11 - (mask & (u64)0x7ffffffffffffU);
  u64 f2_ = f21 - (mask & (u64)0x7ffffffffffffU);
  u64 f3_ = f31 - (mask & (u64)0x7ffffffffffffU);
  u64 f4_ = f41 - (mask & (u64)0x7ffffffffffffU);
  u64 f02 = f0_;
  u64 f12 = f1_;
  u64 f22 = f2_;
  u64 f32 = f3_;
  u64 f42 = f4_;
  u64 o00 = f02 | f12 << (u32)51U;
  u64 o10 = f12 >> (u32)13U | f22 << (u32)38U;
  u64 o20 = f22 >> (u32)26U | f32 << (u32)25U;
  u64 o30 = f32 >> (u32)39U | f42 << (u32)12U;
  u64 o0 = o00;
  u64 o1 = o10;
  u64 o2 = o20;
  u64 o3 = o30;
  u64s[0U] = o0;
  u64s[1U] = o1;
  u64s[2U] = o2;
  u64s[3U] = o3;
}

static inline void Hacl_Impl_Curve25519_Field51_cswap2(u64 bit, u64 *p1, u64 *p2)
{
  u64 mask = (u64)0U - bit;
  u32 i;
  for (i = (u32)0U; i < (u32)10U; i++)
  {
    u64 dummy = mask & (p1[i] ^ p2[i]);
    p1[i] = p1[i] ^ dummy;
    p2[i] = p2[i] ^ dummy;
  }
}
