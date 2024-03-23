// RUN: %clang_cc1 -target-abi experimental-a %s -emit-llvm -o - \
// RUN:   | FileCheck %s

// Basic argument/attribute and return tests for the "Experimental A" ABI.

typedef struct {
  short aa;
} one_value;

typedef struct {
  short aa;
  short bb;
} two_values;

typedef struct {
  short aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, kk, ll, mm, nn, oo;
} fifteen_values;

typedef struct {
  short aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp;
} sixteen_values;

typedef struct {
  short aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, kk, ll, mm, nn, oo, pp, qq;
} seventeen_values;

typedef struct {
    unsigned char tag;
    union {
        two_values j0;
        fifteen_values j1;
    } u;
} small_variant;

typedef struct {
    unsigned char tag;
    union {
        two_values j2;
        sixteen_values j3;
    } u;
} big_variant;

// CHECK: define void @take_one_value(i16 %a.coerce)
void take_one_value(one_value a) {}

// CHECK: define void @take_two_values(i16 %a.0, i16 %a.1)
void take_two_values(two_values a) {}

// CHECK: define void @take_sixteen_values(i16 %a.0, i16 %a.1, i16 %a.2, i16 %a.3, i16 %a.4, i16 %a.5, i16 %a.6, i16 %a.7, i16 %a.8, i16 %a.9, i16 %a.10, i16 %a.11, i16 %a.12, i16 %a.13, i16 %a.14, i16 %a.15)
void take_sixteen_values(sixteen_values a) {}

// CHECK: define void @take_seventeen_values(ptr noundef byval(%struct.seventeen_values) align 2 %a)
void take_seventeen_values(seventeen_values a) {}

// CHECK: define void @take_small_variant(i8 %a.0, i16 %a.1, i16 %a.2, i16 %a.3, i16 %a.4, i16 %a.5, i16 %a.6, i16 %a.7, i16 %a.8, i16 %a.9, i16 %a.10, i16 %a.11, i16 %a.12, i16 %a.13, i16 %a.14, i16 %a.15)
void take_small_variant(small_variant a) {}

// CHECK: define void @take_big_variant(ptr noundef byval(%struct.big_variant) align 2 %a)
void take_big_variant(big_variant a) {}

// CHECK: define i16 @return_one_value()
one_value return_one_value(void) { return (one_value) {}; }

// CHECK: define %struct.two_values @return_two_values()
two_values return_two_values(void) { return (two_values) {}; }

// CHECK: define void @return_sixteen_values(ptr {{.*}} sret(%struct.sixteen_values) align 2 %agg.result)
sixteen_values return_sixteen_values(void) { return (sixteen_values) {}; }

// CHECK: define void @return_seventeen_values(ptr {{.*}} sret(%struct.seventeen_values) align 2 %agg.result)
seventeen_values return_seventeen_values(void) { return (seventeen_values) {}; }

// CHECK: define void @return_small_variant(ptr {{.*}} sret(%struct.small_variant) align 2 %agg.result)
small_variant return_small_variant(void) { return (small_variant) {}; }

// CHECK: define void @return_big_variant(ptr {{.*}} sret(%struct.big_variant) align 2 %agg.result)
big_variant return_big_variant(void) { return (big_variant) {}; }
