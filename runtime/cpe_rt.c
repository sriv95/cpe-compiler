/* Runtime for compiled CPE programs.

   Value representation (every value is one 64-bit word):
   - immediate: (n << 1) | 1.  Integer, Char, Bool (False=0, True=1), unit (0)
     and nullary constructors (their tag) are immediates.
   - otherwise a pointer to a block: word 0 is a header (size << 8 | tag),
     followed by `size` fields.
       constructor block : tag = constructor index (pairs use 0, (::) uses 1)
       DOUBLE (250)      : field 0 = raw IEEE bits
       CLOSURE (251)     : code, arity, ncaptured, captured values...
       PAP (252)         : target closure, nargs, args...
   There is no garbage collector: blocks are bump-allocated and never freed. */
#include <math.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int64_t v;

#define TAG_DOUBLE 250
#define TAG_CLOSURE 251
#define TAG_PAP 252

#define IMM(n) ((v)((((uint64_t)(n)) << 1) | 1))
#define UNIMM(x) ((x) >> 1)
#define IS_IMM(x) ((x)&1)
#define BLK(x) ((int64_t *)(x))
#define HDR(x) (BLK(x)[0])
#define TAG(x) ((int)(HDR(x) & 255))
#define SIZE(x) ((int64_t)(HDR(x) >> 8))
#define FIELD(x, i) (BLK(x)[(i) + 1])

#define TRUE_V IMM(1)
#define FALSE_V IMM(0)

/* provided by the generated code */
extern v cpe_user_main(v env, v args);
extern v cpe_init(v env);

static void fatal(const char *msg) {
  fflush(stdout);
  fprintf(stderr, "error: %s\n", msg);
  exit(1);
}

/* ---------- allocation ---------- */

static char *arena_ptr = 0;
static char *arena_end = 0;

v cpe_alloc(int64_t nfields, int64_t tag) {
  size_t bytes = (size_t)(nfields + 1) * 8;
  if (arena_ptr == 0 || (size_t)(arena_end - arena_ptr) < bytes) {
    size_t chunk = (size_t)1 << 24;
    if (bytes > chunk) chunk = bytes;
    arena_ptr = (char *)malloc(chunk);
    if (!arena_ptr) fatal("out of memory");
    arena_end = arena_ptr + chunk;
  }
  int64_t *p = (int64_t *)arena_ptr;
  arena_ptr += bytes;
  p[0] = (nfields << 8) | tag;
  return (v)p;
}

static v mk_double(double d) {
  v b = cpe_alloc(1, TAG_DOUBLE);
  memcpy(&FIELD(b, 0), &d, 8);
  return b;
}

static double get_double(v x) {
  double d;
  memcpy(&d, &FIELD(x, 0), 8);
  return d;
}

static v cons(v h, v t) {
  v c = cpe_alloc(2, 1);
  FIELD(c, 0) = h;
  FIELD(c, 1) = t;
  return c;
}

/* ---------- errors ---------- */

static void print_list_string(FILE *f, v s) {
  while (s != IMM(0)) {
    fputc((int)UNIMM(FIELD(s, 0)), f);
    s = FIELD(s, 1);
  }
}

void cpe_error(v msg) {
  fflush(stdout);
  fputs("error: ", stderr);
  print_list_string(stderr, msg);
  fputc('\n', stderr);
  exit(1);
}

void cpe_match_fail(void) { fatal("non-exhaustive patterns"); }

/* ---------- arithmetic ---------- */

v cpe_idiv(v a, v b) {
  int64_t x = UNIMM(a), y = UNIMM(b);
  if (y == 0) fatal("division by zero");
  return IMM(x / y);
}

v cpe_imod(v a, v b) {
  int64_t x = UNIMM(a), y = UNIMM(b);
  if (y == 0) fatal("division by zero");
  return IMM(x % y);
}

/* Double // Integer : Integer */
v cpe_ddivi(v a, v b) {
  int64_t y = UNIMM(b);
  if (y == 0) fatal("division by zero");
  return IMM((int64_t)(get_double(a) / (double)y));
}

/* Double % Integer : Double */
v cpe_dmodi(v a, v b) {
  int64_t y = UNIMM(b);
  if (y == 0) fatal("division by zero");
  return mk_double(fmod(get_double(a), (double)y));
}

v cpe_box_double_bits(int64_t bits) {
  v b = cpe_alloc(1, TAG_DOUBLE);
  FIELD(b, 0) = bits;
  return b;
}

/* ---------- structural equality ---------- */

static int eq(v a, v b) {
  for (;;) {
    if (a == b) return 1;
    if (IS_IMM(a) || IS_IMM(b)) return 0;
    int ta = TAG(a), tb = TAG(b);
    if (ta != tb) return 0;
    if (ta == TAG_DOUBLE) return get_double(a) == get_double(b);
    if (ta == TAG_CLOSURE || ta == TAG_PAP)
      fatal("cannot compare functions");
    int64_t n = SIZE(a);
    if (n != SIZE(b)) return 0;
    if (n == 0) return 1;
    for (int64_t i = 0; i + 1 < n; i++)
      if (!eq(FIELD(a, i), FIELD(b, i))) return 0;
    /* iterate on the last field so that long lists do not use the stack */
    a = FIELD(a, n - 1);
    b = FIELD(b, n - 1);
  }
}

v cpe_eq(v a, v b) { return eq(a, b) ? TRUE_V : FALSE_V; }

/* ---------- lists ---------- */

v cpe_append(v a, v b) {
  if (a == IMM(0)) return b;
  v head = cons(FIELD(a, 0), IMM(0));
  v last = head;
  a = FIELD(a, 1);
  while (a != IMM(0)) {
    v c = cons(FIELD(a, 0), IMM(0));
    FIELD(last, 1) = c;
    last = c;
    a = FIELD(a, 1);
  }
  FIELD(last, 1) = b;
  return head;
}

/* ---------- closures and application ---------- */

typedef v (*f1)(v, v);
typedef v (*f2)(v, v, v);
typedef v (*f3)(v, v, v, v);
typedef v (*f4)(v, v, v, v, v);
typedef v (*f5)(v, v, v, v, v, v);
typedef v (*f6)(v, v, v, v, v, v, v);
typedef v (*f7)(v, v, v, v, v, v, v, v);
typedef v (*f8)(v, v, v, v, v, v, v, v, v);
typedef v (*f9)(v, v, v, v, v, v, v, v, v, v);
typedef v (*f10)(v, v, v, v, v, v, v, v, v, v, v);
typedef v (*f11)(v, v, v, v, v, v, v, v, v, v, v, v);
typedef v (*f12)(v, v, v, v, v, v, v, v, v, v, v, v, v);

static v call_code(v clo, const v *a, int64_t k) {
  void *c = (void *)FIELD(clo, 0);
  switch (k) {
  case 1: return ((f1)c)(clo, a[0]);
  case 2: return ((f2)c)(clo, a[0], a[1]);
  case 3: return ((f3)c)(clo, a[0], a[1], a[2]);
  case 4: return ((f4)c)(clo, a[0], a[1], a[2], a[3]);
  case 5: return ((f5)c)(clo, a[0], a[1], a[2], a[3], a[4]);
  case 6: return ((f6)c)(clo, a[0], a[1], a[2], a[3], a[4], a[5]);
  case 7: return ((f7)c)(clo, a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
  case 8: return ((f8)c)(clo, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
  case 9:
    return ((f9)c)(clo, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
  case 10:
    return ((f10)c)(clo, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8],
                    a[9]);
  case 11:
    return ((f11)c)(clo, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8],
                    a[9], a[10]);
  case 12:
    return ((f12)c)(clo, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8],
                    a[9], a[10], a[11]);
  default: fatal("functions with more than 12 parameters are not supported");
  }
  return 0;
}

/* apply f to n arguments */
v cpe_apply(v f, int64_t n, const v *args) {
  for (;;) {
    if (IS_IMM(f)) fatal("application of a non-function");
    int tag = TAG(f);
    if (tag == TAG_CLOSURE) {
      int64_t arity = FIELD(f, 1);
      if (n == arity) return call_code(f, args, n);
      if (n < arity) {
        v p = cpe_alloc(2 + n, TAG_PAP);
        FIELD(p, 0) = f;
        FIELD(p, 1) = n;
        for (int64_t i = 0; i < n; i++) FIELD(p, 2 + i) = args[i];
        return p;
      }
      f = call_code(f, args, arity);
      args += arity;
      n -= arity;
    } else if (tag == TAG_PAP) {
      int64_t k = FIELD(f, 1);
      v *all = (v *)malloc((size_t)(k + n) * sizeof(v));
      if (!all) fatal("out of memory");
      for (int64_t i = 0; i < k; i++) all[i] = FIELD(f, 2 + i);
      for (int64_t i = 0; i < n; i++) all[k + i] = args[i];
      v r = cpe_apply(FIELD(f, 0), k + n, all);
      free(all);
      return r;
    } else {
      fatal("application of a non-function");
    }
  }
}

/* g . f  as a closure of arity 1 capturing g and f */
static v compose_code(v env, v x) {
  v fx = cpe_apply(FIELD(env, 3 + 1), 1, &x);
  return cpe_apply(FIELD(env, 3 + 0), 1, &fx);
}

v cpe_compose(v g, v f) {
  v c = cpe_alloc(3 + 2, TAG_CLOSURE);
  FIELD(c, 0) = (v)(void *)compose_code;
  FIELD(c, 1) = 1;
  FIELD(c, 2) = 2;
  FIELD(c, 3) = g;
  FIELD(c, 4) = f;
  return c;
}

/* ---------- program entry ---------- */

static int g_argc;
static char **g_argv;
static int g_status;

static v string_of_c(const char *s) {
  size_t n = strlen(s);
  v r = IMM(0);
  for (size_t i = n; i > 0; i--) r = cons(IMM((unsigned char)s[i - 1]), r);
  return r;
}

static void *run(void *unused) {
  (void)unused;
  v args = IMM(0);
  for (int i = g_argc - 1; i >= 1; i--) args = cons(string_of_c(g_argv[i]), args);
  cpe_init(0);
  v result = cpe_user_main(0, args);
  print_list_string(stdout, result);
  fputc('\n', stdout);
  fflush(stdout);
  g_status = 0;
  return 0;
}

int main(int argc, char **argv) {
  g_argc = argc;
  g_argv = argv;
  /* deep (non-tail) recursion over long lists needs a big stack */
  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setstacksize(&attr, (size_t)1 << 30);
  pthread_t th;
  if (pthread_create(&th, &attr, run, 0) != 0) {
    run(0);
  } else {
    pthread_join(th, 0);
  }
  return g_status;
}
