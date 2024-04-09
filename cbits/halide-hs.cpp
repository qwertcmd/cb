// The only Halide header file you need is Halide.h. It includes all of Halide.
#include "Halide.h"

using namespace Halide;

// Turn a pointer of argument points (from haskell) into a vector of
// arguments used for ahead of time compilation.
std::vector<Argument> argvector (int n, Argument** args) {
  std::vector<Argument> values; //(args, args+n);
  for (int i = 0; i < n; i++) {
    values.push_back(*(args[i]));
  }
  return values;
}

extern "C" {

/** Arguments *********************************************************/

/* Param<>* new_scalar_param(halide_type_t ty, char* name) { */
/*   return new Param<uint_8>(ty, name); */
/* } */

// must be a better way to do this?
void delete_param_int8(Param<int8_t>* p) { delete p; }
void delete_param_int16(Param<int16_t>* p) { delete p; }
void delete_param_int32(Param<int32_t>* p) { delete p; }
void delete_param_int64(Param<int64_t>* p) { delete p; }
void delete_param_uint8(Param<uint8_t>* p) { delete p; }
void delete_param_uint16(Param<uint16_t>* p) { delete p; }
void delete_param_uint32(Param<uint32_t>* p) { delete p; }
void delete_param_uint64(Param<uint64_t>* p) { delete p; }
void delete_param_float(Param<float>* p) { delete p; }
void delete_param_double(Param<double>* p) { delete p; }

ImageParam* new_image_param(halide_type_t ty, int dim, char* name) {
  ImageParam* img = new ImageParam(ty, dim, name);
  return img;
}

/* void set_image_param(ImageParam* img, halide_buffer_t* buff) { */
/*   Image<> */

void delete_image_param(ImageParam* img) {
  delete img;
}

/* Argument* scalar_arg(Param* p) { */
/*   return new Argument(*p); */
/* } */

Argument* image_arg(ImageParam* img) {
  return new Argument(*img);
}

Argument* image_arg_float(ImageParam* p) {
  Argument* a = new Argument(*p);
  return a;
}

void delete_arg(Argument* a) {
  delete a;
}

/** Buffers ***********************************************************/

Realization* from_new_buffers(int32_t n, halide_buffer_t* buffer_ptr) {
  std::vector<Buffer<>> buffer_v;
  for (int i=0; i<n; i++) {
    buffer_v.push_back(Buffer<>(buffer_ptr[i]));
  }
  return new Realization(buffer_v);
}

/** Scheduling ********************************************************/

void parallel_func(Func* f, char* nm) {
  f->parallel(Var(nm));
}

void vectorise_func(Func* f, char* nm, int32_t n) {
  f->vectorize(Var(nm), n);
}

/** Realizations ******************************************************/

Realization* from_buffers(int32_t n, int32_t* ts, buffer_t* buffer_ts) {

  std::vector<Buffer<>> buffer_v;

  for (int i = 0; i < n; i++) {
    int32_t t = ts[i];
    Buffer<> b;
    switch (t) {
      case 0  : b = Buffer<uint8_t>(buffer_ts[i]);  break;
      case 1  : b = Buffer<uint16_t>(buffer_ts[i]); break;
      case 2  : b = Buffer<uint32_t>(buffer_ts[i]); break;
      case 3  : b = Buffer<uint16_t>(buffer_ts[i]); break;
      case 4  : b = Buffer<int8_t>(buffer_ts[i]);   break;
      case 5  : b = Buffer<int16_t>(buffer_ts[i]);  break;
      case 6  : b = Buffer<int32_t>(buffer_ts[i]);  break;
      case 7  : b = Buffer<int64_t>(buffer_ts[i]);  break;
      case 8  : b = Buffer<float>(buffer_ts[i]);    break;
      case 9  : b = Buffer<double>(buffer_ts[i]);   break;
      default : b = Buffer<void*>(buffer_ts[i]);
    }
    buffer_v.push_back(b);
  }

  Realization* xs = new Realization(buffer_v);
  return xs;
}

void realize(Func* f, Realization* dst) {
  f->realize(*dst);
}

Var* new_var(char* var_name) {
  return new Var(var_name);
}

void delete_var(Var* var) {
  delete var;
}

/** Target ************************************************************/

/* Target* new_target */
/*   (Target::OS o, */
/*    Target::Arch a, */
/*    int target_bits, */
/*    int num_features, */
/*    Target::Feature* features_ptr) { */
/*   std::vector<Target::Feature> features_vector; */
/*   for (int i = 0; i < num_features; i++) { */
/*     features_vector.push_back(features_ptr[i]); */
/*   } */
/*   return new Target(o,a,target_bits,features_vector); */
/* } */

Target* new_target (Target::OS o, Target::Arch a, int b, uint64_t features) {
  std::vector<Target::Feature> features_vector;

  for (uint64_t i = 0; i <= Target::FeatureEnd; i++) {
    if (features & (1ull << i)) {
      Target::Feature f = static_cast<Target::Feature>(i);
      features_vector.push_back(f);
    }
  }

  return new Target(o,a,b,features_vector);
}

Target* target_from_string(char* target_string) {
  return new Target(target_string);
}

void target_string(Target* target, char* string_ptr) {
  strcpy(string_ptr, target->to_string().c_str());
}

Target* host_target() {
  Target* t = new Target;
  *t = get_host_target();
  return t;
}

Target* environment_target() {
  Target* t = new Target;
  *t = get_target_from_environment();
  return t;
}

void from_target(
  Target* target,
  Target::OS* o_ptr,
  Target::Arch* a_ptr,
  int* bits_ptr,
  uint64_t* features_ptr) {

  *o_ptr = target->os;
  *a_ptr = target->arch;
  *bits_ptr = target->bits;

  uint64_t features = 0;
  for (int i = 0; i < Target::FeatureEnd; i ++) {
    Target::Feature f = static_cast<Target::Feature>(i);
    if (target->has_feature(f)) {
      features |= 1 << i;
    }
  }
  *features_ptr = features;
}

void delete_target(Target* target) {
  delete target;
}



/** Funcs *************************************************************/

Func* new_func_named(char* name) {
  Func* f = new Func(name);
  return f;
}

Func* new_func_unnamed() {
  Func* f = new Func();
  return f;
}

void delete_func(Func* f) {
  delete f;
}

void to_assembly(Func* f, char* filepath, int n, Argument** args, char* name, Target* t) {
  f->compile_to_assembly(filepath, argvector(n,args), name, *t);
}

void to_object(Func* f, char* filepath, int n, Argument** args, char* name, Target* t) {
  f->compile_to_object(filepath, argvector(n,args), name, *t);
}

void to_bitcode(Func* f, char* filepath, int n, Argument** args, char* name, Target* t) {
  f->compile_to_bitcode(filepath, argvector(n,args), name, *t);
}

void to_lowered_stmt(Func* f, char* filepath, int n, Argument** args, Target* t) {
  f->compile_to_lowered_stmt(filepath, argvector(n,args), Text, *t);
}

void to_file(Func* f, char* filepath, int n, Argument** args, char* name, Target* t) {
  f->compile_to_file(filepath, argvector(n,args), name, *t);
}

} // extern "C"
