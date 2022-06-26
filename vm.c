//> A Virtual Machine vm-c
//> Types of Values include-stdarg
#include <bits/stdc++.h>
#include <stdlib.h>
#include <stdarg.h>
//< Types of Values include-stdarg
//> vm-include-stdio
#include <stdio.h>
//> Strings vm-include-string
#include <string.h>
//< Strings vm-include-string
//> Calls and Functions vm-include-time
#include <time.h>
//< Calls and Functions vm-include-time

//< vm-include-stdio
#include "common.h"
//> Scanning on Demand vm-include-compiler
#include "compiler.h"
//< Scanning on Demand vm-include-compiler
//> vm-include-debug
#include "debug.h"
//< vm-include-debug
//> Strings vm-include-object-memory
#include "object.h"
#include "memory.h"
//< Strings vm-include-object-memory
#include "vm.h"

using namespace std;

VM vm; // [one]
//> Calls and Functions clock-native
static Value clockNative(int argCount, Value* args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value clear(int argCount, Value* args) {
	for(int i = 0; i < 25; ++i) printf("\n");
	return NUMBER_VAL(0);
}

static Value intInput(int argCount, Value* args) {
	int n;
	scanf("%d", &n);
	return NUMBER_VAL(n);
}

static Value floatInput(int argCount, Value* args) {
	float n;
	scanf("%f", &n);
	return NUMBER_VAL(n);
}


static Value totient(int argCount, Value* args) {
    int n = AS_NUMBER(*args);
    int result = n;
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            while (n % i == 0)
                n /= i;
            result -= result / i;
        }
    }
    if (n > 1)
        result -= result / n;
        
    return NUMBER_VAL(result);
}

static Value fisr(int argCount, Value* args) {
	 long i;
	 float number = AS_NUMBER(*args);
	// printf("%f\n", number);
        float x2, y;
        const float threehalfs = 1.5F;
 
        x2 = number * 0.5F;
        y  = number;
        i  = * ( long * ) &y;                       // evil floating point bit level hacking
        i  = 0x5f3759df - ( i >> 1 );               // what the fuck?
        y  = * ( float * ) &i;
        y  = y * ( threehalfs - ( x2 * y * y ) );   // 1st iteration
 //	printf("%f\n", y);
        return NUMBER_VAL(y);
}


double sqrt_newton(double n) {
    const double eps = 1E-15;
    double x = 1;
    for (;;) {
        double nx = (x + n / x) / 2;
        if (abs(x - nx) < eps)
            break;
        x = nx;
    }
    printf("%f %f\n", n, x);
    return x;
}

static Value sqrt(int argCount, Value* args) {
	double d = AS_NUMBER(*args);
	return NUMBER_VAL(sqrt_newton(d));
}

const int N = 1000 * 1000; // number of steps (already multiplied by 2)

int f(int a) {
	return 1 / a;
}



static Value integrate(int argCount, Value* args) {
	double a = AS_NUMBER(*args), b = AS_NUMBER(*(args + 1));
	double h = (b - a) / N;
    double s = f(a) + f(b); // a = x_0 and b = x_2n
    for (int i = 1; i <= N - 1; ++i) { // Refer to final Simpson's formula
        double x = a + h * i;
        s += f(x) * ((i & 1) ? 4 : 2);
    }
    s *= h / 3;
	return NUMBER_VAL(s);
}

int solve(int a, int b, int m) {
    a %= m, b %= m;
    int n = sqrt(m) + 1;

    int an = 1;
    for (int i = 0; i < n; ++i)
        an = (an * 1ll * a) % m;

    unordered_map<int, int> vals;
    for (int q = 0, cur = b; q <= n; ++q) {
        vals[cur] = q;
        cur = (cur * 1ll * a) % m;
    }

    for (int p = 1, cur = 1; p <= n; ++p) {
        cur = (cur * 1ll * an) % m;
        if (vals.count(cur)) {
            int ans = n * p - vals[cur];
            return ans;
        }
    }
    return -1;
}

static Value discreteLog(int argCount, Value* args) {
	    int a = AS_NUMBER(*(args)), b = AS_NUMBER(*(args + 1)), m = AS_NUMBER(*(args + 2));
	    a %= m, b %= m;
	    int n = sqrt(m) + 1;

	    int an = 1;
	    for (int i = 0; i < n; ++i)
		an = (an * 1ll * a) % m;

	    unordered_map<int, int> vals;
	    for (int q = 0, cur = b; q <= n; ++q) {
		vals[cur] = q;
		cur = (cur * 1ll * a) % m;
	    }

	    for (int p = 2, cur = 2; p <= n; ++p) {
		cur = (cur * 1ll * an) % m;
		if (vals.count(cur)) {
		    int ans = n * p - vals[cur];
		    return NUMBER_VAL(ans);
		}
	    }
	    return NUMBER_VAL(-1);
}

int powmod (int a, int b, int p) {
    int res = 1;
    while (b)
        if (b & 1)
            res = int (res * 1ll * a % p),  --b;
        else
            a = int (a * 1ll * a % p),  b >>= 1;
    return res;
}


static Value generator(int argCount, Value* args) {
	int p = AS_NUMBER(*args);
	vector<int> fact;
    int phi = p-1,  n = phi;
    for (int i=2; i*i<=n; ++i)
        if (n % i == 0) {
            fact.push_back (i);
            while (n % i == 0)
                n /= i;
        }
    if (n > 1)
        fact.push_back (n);

    for (int res=2; res<=p; ++res) {
        bool ok = true;
        for (size_t i=0; i<fact.size() && ok; ++i)
            ok &= powmod (res, phi / fact[i], p) != 1;
        if (ok)  return NUMBER_VAL(res);
    }
    return NUMBER_VAL(-1);
}




int binpower(int base, int e, int mod) {
    int result = 1;
    base %= mod;
    while (e) {
        if (e & 1)
            result = (int)result * base % mod;
        base = (int)base * base % mod;
        e >>= 1;
    }
    return result;
}

static Value probablyPrimeFermat(int argCount, Value* args) {
	int iter = 5, n = AS_NUMBER(*args);
	if (n < 4)
        return BOOL_VAL(n == 2 || n == 3);

    for (int i = 0; i < iter; i++) {
        int a = 2 + rand() % (n - 3);
        if (binpower(a, n - 1, n) != 1) 
            return BOOL_VAL(false);
    }
    return BOOL_VAL(true);
}
//< Calls and Functions clock-native
//> reset-stack
static void resetStack() {
  vm.stackTop = vm.stack;
//> Calls and Functions reset-frame-count
  vm.frameCount = 0;
//< Calls and Functions reset-frame-count
//> Closures init-open-upvalues
  vm.openUpvalues = NULL;
//< Closures init-open-upvalues
}
//< reset-stack
//> Types of Values runtime-error
static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

/* Types of Values runtime-error < Calls and Functions runtime-error-temp
  size_t instruction = vm.ip - vm.chunk->code - 1;
  int line = vm.chunk->lines[instruction];
*/
/* Calls and Functions runtime-error-temp < Calls and Functions runtime-error-stack
  CallFrame* frame = &vm.frames[vm.frameCount - 1];
  size_t instruction = frame->ip - frame->function->chunk.code - 1;
  int line = frame->function->chunk.lines[instruction];
*/
/* Types of Values runtime-error < Calls and Functions runtime-error-stack
  fprintf(stderr, "[line %d] in script\n", line);
*/
//> Calls and Functions runtime-error-stack
  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame* frame = &vm.frames[i];
/* Calls and Functions runtime-error-stack < Closures runtime-error-function
    ObjFunction* function = frame->function;
*/
//> Closures runtime-error-function
    ObjFunction* function = frame->closure->function;
//< Closures runtime-error-function
    size_t instruction = frame->ip - function->chunk.code - 1;
    fprintf(stderr, "[line %d] in ", // [minus]
            function->chunk.lines[instruction]);
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }

//< Calls and Functions runtime-error-stack
  resetStack();
}
//< Types of Values runtime-error
//> Calls and Functions define-native
static void defineNative(const char* name, NativeFn function) {
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(newNative(function)));
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  pop();
  pop();
}
//< Calls and Functions define-native

void initVM() {
//> call-reset-stack
  resetStack();
//< call-reset-stack
//> Strings init-objects-root
  vm.objects = NULL;
//< Strings init-objects-root
//> Garbage Collection init-gc-fields
  vm.bytesAllocated = 0;
  vm.nextGC = 1024 * 1024;
//< Garbage Collection init-gc-fields
//> Garbage Collection init-gray-stack

  vm.grayCount = 0;
  vm.grayCapacity = 0;
  vm.grayStack = NULL;
//< Garbage Collection init-gray-stack
//> Global Variables init-globals

  initTable(&vm.globals);
//< Global Variables init-globals
//> Hash Tables init-strings
  initTable(&vm.strings);
//< Hash Tables init-strings
//> Methods and Initializers init-init-string

//> null-init-string
  vm.initString = NULL;
//< null-init-string
  vm.initString = copyString("init", 4);
//< Methods and Initializers init-init-string
//> Calls and Functions define-native-clock

  defineNative("clock", clockNative);
  defineNative("i_sqrt", fisr);
  defineNative("is_prime", probablyPrimeFermat);
  defineNative("totient", totient);
  defineNative("sqrt", sqrt);
  defineNative("integrate", integrate);
  defineNative("discrete_log", discreteLog);
  defineNative("primitive_root", generator);
  defineNative("clear", clear);
  defineNative("intInput", intInput);
  defineNative("floatInput", floatInput);	
//< Calls and Functions define-native-clock
}

void freeVM() {
//> Global Variables free-globals
  freeTable(&vm.globals);
//< Global Variables free-globals
//> Hash Tables free-strings
  freeTable(&vm.strings);
//< Hash Tables free-strings
//> Methods and Initializers clear-init-string
  vm.initString = NULL;
//< Methods and Initializers clear-init-string
//> Strings call-free-objects
  freeObjects();
//< Strings call-free-objects
}
//> push
void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}
//< push
//> pop
Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}
//< pop
//> Types of Values peek
static Value peek(int distance) {
  return vm.stackTop[-1 - distance];
}
//< Types of Values peek
/* Calls and Functions call < Closures call-signature
static bool call(ObjFunction* function, int argCount) {
*/
//> Calls and Functions call
//> Closures call-signature
static bool call(ObjClosure* closure, int argCount) {
//< Closures call-signature
/* Calls and Functions check-arity < Closures check-arity
  if (argCount != function->arity) {
    runtimeError("Expected %d arguments but got %d.",
        function->arity, argCount);
*/
//> Closures check-arity
  if (argCount != closure->function->arity) {
    runtimeError("Expected %d arguments but got %d.",
        closure->function->arity, argCount);
//< Closures check-arity
//> check-arity
    return false;
  }

//< check-arity
//> check-overflow
  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }

//< check-overflow
  CallFrame* frame = &vm.frames[vm.frameCount++];
/* Calls and Functions call < Closures call-init-closure
  frame->function = function;
  frame->ip = function->chunk.code;
*/
//> Closures call-init-closure
  frame->closure = closure;
  frame->ip = closure->function->chunk.code;
//< Closures call-init-closure
  frame->slots = vm.stackTop - argCount - 1;
  return true;
}
//< Calls and Functions call
//> Calls and Functions call-value
static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
//> Methods and Initializers call-bound-method
      case OBJ_BOUND_METHOD: {
        ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
//> store-receiver
        vm.stackTop[-argCount - 1] = bound->receiver;
//< store-receiver
        return call(bound->method, argCount);
      }
//< Methods and Initializers call-bound-method
//> Classes and Instances call-class
      case OBJ_CLASS: {
        ObjClass* klass = AS_CLASS(callee);
        vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));
//> Methods and Initializers call-init
        Value initializer;
        if (tableGet(&klass->methods, vm.initString,
                     &initializer)) {
          return call(AS_CLOSURE(initializer), argCount);
//> no-init-arity-error
        } else if (argCount != 0) {
          runtimeError("Expected 0 arguments but got %d.",
                       argCount);
          return false;
//< no-init-arity-error
        }
//< Methods and Initializers call-init
        return true;
      }
//< Classes and Instances call-class
//> Closures call-value-closure
      case OBJ_CLOSURE:
        return call(AS_CLOSURE(callee), argCount);
//< Closures call-value-closure
/* Calls and Functions call-value < Closures call-value-closure
      case OBJ_FUNCTION: // [switch]
        return call(AS_FUNCTION(callee), argCount);
*/
//> call-native
      case OBJ_NATIVE: {
        NativeFn native = AS_NATIVE(callee);
        Value result = native(argCount, vm.stackTop - argCount);
        vm.stackTop -= argCount + 1;
        push(result);
        return true;
      }
//< call-native
      default:
        break; // Non-callable object type.
    }
  }
  runtimeError("Can only call functions and classes.");
  return false;
}
//< Calls and Functions call-value
//> Methods and Initializers invoke-from-class
static bool invokeFromClass(ObjClass* klass, ObjString* name,
                            int argCount) {
  Value method;
  if (!tableGet(&klass->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars);
    return false;
  }
  return call(AS_CLOSURE(method), argCount);
}
//< Methods and Initializers invoke-from-class
//> Methods and Initializers invoke
static bool invoke(ObjString* name, int argCount) {
  Value receiver = peek(argCount);
//> invoke-check-type

  if (!IS_INSTANCE(receiver)) {
    runtimeError("Only instances have methods.");
    return false;
  }

//< invoke-check-type
  ObjInstance* instance = AS_INSTANCE(receiver);
//> invoke-field

  Value value;
  if (tableGet(&instance->fields, name, &value)) {
    vm.stackTop[-argCount - 1] = value;
    return callValue(value, argCount);
  }

//< invoke-field
  return invokeFromClass(instance->klass, name, argCount);
}
//< Methods and Initializers invoke
//> Methods and Initializers bind-method
static bool bindMethod(ObjClass* klass, ObjString* name) {
  Value method;
  if (!tableGet(&klass->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars);
    return false;
  }

  ObjBoundMethod* bound = newBoundMethod(peek(0),
                                         AS_CLOSURE(method));
  pop();
  push(OBJ_VAL(bound));
  return true;
}
//< Methods and Initializers bind-method
//> Closures capture-upvalue
static ObjUpvalue* captureUpvalue(Value* local) {
//> look-for-existing-upvalue
  ObjUpvalue* prevUpvalue = NULL;
  ObjUpvalue* upvalue = vm.openUpvalues;
  while (upvalue != NULL && upvalue->location > local) {
    prevUpvalue = upvalue;
    upvalue = upvalue->next;
  }

  if (upvalue != NULL && upvalue->location == local) {
    return upvalue;
  }

//< look-for-existing-upvalue
  ObjUpvalue* createdUpvalue = newUpvalue(local);
//> insert-upvalue-in-list
  createdUpvalue->next = upvalue;

  if (prevUpvalue == NULL) {
    vm.openUpvalues = createdUpvalue;
  } else {
    prevUpvalue->next = createdUpvalue;
  }

//< insert-upvalue-in-list
  return createdUpvalue;
}
//< Closures capture-upvalue
//> Closures close-upvalues
static void closeUpvalues(Value* last) {
  while (vm.openUpvalues != NULL &&
         vm.openUpvalues->location >= last) {
    ObjUpvalue* upvalue = vm.openUpvalues;
    upvalue->closed = *upvalue->location;
    upvalue->location = &upvalue->closed;
    vm.openUpvalues = upvalue->next;
  }
}
//< Closures close-upvalues
//> Methods and Initializers define-method
static void defineMethod(ObjString* name) {
  Value method = peek(0);
  ObjClass* klass = AS_CLASS(peek(1));
  tableSet(&klass->methods, name, method);
  pop();
}
//< Methods and Initializers define-method
//> Types of Values is-falsey
static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}
//< Types of Values is-falsey
//> Strings concatenate
static void concatenate() {
/* Strings concatenate < Garbage Collection concatenate-peek
  ObjString* b = AS_STRING(pop());
  ObjString* a = AS_STRING(pop());
*/
//> Garbage Collection concatenate-peek
  ObjString* b = AS_STRING(peek(0));
  ObjString* a = AS_STRING(peek(1));
//< Garbage Collection concatenate-peek

  int length = a->length + b->length;
  char* chars = ALLOCATE(char, length + 1);
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString* result = takeString(chars, length);
//> Garbage Collection concatenate-pop
  pop();
  pop();
//< Garbage Collection concatenate-pop
  push(OBJ_VAL(result));
}
//< Strings concatenate
//> run
static InterpretResult run() {
//> Calls and Functions run
  CallFrame* frame = &vm.frames[vm.frameCount - 1];

/* A Virtual Machine run < Calls and Functions run
#define READ_BYTE() (*vm.ip++)
*/
#define READ_BYTE() (*frame->ip++)
/* A Virtual Machine read-constant < Calls and Functions run
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
*/

/* Jumping Back and Forth read-short < Calls and Functions run
#define READ_SHORT() \
    (vm.ip += 2, (uint16_t)((vm.ip[-2] << 8) | vm.ip[-1]))
*/
#define READ_SHORT() \
    (frame->ip += 2, \
    (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

/* Calls and Functions run < Closures read-constant
#define READ_CONSTANT() \
    (frame->function->chunk.constants.values[READ_BYTE()])
*/
//> Closures read-constant
#define READ_CONSTANT() \
    (frame->closure->function->chunk.constants.values[READ_BYTE()])
//< Closures read-constant

//< Calls and Functions run
//> Global Variables read-string
#define READ_STRING() AS_STRING(READ_CONSTANT())
//< Global Variables read-string
/* A Virtual Machine binary-op < Types of Values binary-op
#define BINARY_OP(op) \
    do { \
      double b = pop(); \
      double a = pop(); \
      push(a op b); \
    } while (false)
*/
//> Types of Values binary-op
#define BINARY_OP(valueType, op) \
    do { \
      if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
        runtimeError("Operands must be numbers."); \
        return INTERPRET_RUNTIME_ERROR; \
      } \
      double b = AS_NUMBER(pop()); \
      double a = AS_NUMBER(pop()); \
      push(valueType(a op b)); \
    } while (false)
//< Types of Values binary-op

  for (;;) {
//> trace-execution
#ifdef DEBUG_TRACE_EXECUTION
//> trace-stack
    printf("          ");
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");
//< trace-stack
/* A Virtual Machine trace-execution < Calls and Functions trace-execution
    disassembleInstruction(vm.chunk,
                           (int)(vm.ip - vm.chunk->code));
*/
/* Calls and Functions trace-execution < Closures disassemble-instruction
    disassembleInstruction(&frame->function->chunk,
        (int)(frame->ip - frame->function->chunk.code));
*/
//> Closures disassemble-instruction
    disassembleInstruction(&frame->closure->function->chunk,
        (int)(frame->ip - frame->closure->function->chunk.code));
//< Closures disassemble-instruction
#endif

//< trace-execution
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
//> op-constant
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
/* A Virtual Machine op-constant < A Virtual Machine push-constant
        printValue(constant);
        printf("\n");
*/
//> push-constant
        push(constant);
//< push-constant
        break;
      }
//< op-constant
//> Types of Values interpret-literals
      case OP_NIL: push(NIL_VAL); break;
      case OP_TRUE: push(BOOL_VAL(true)); break;
      case OP_FALSE: push(BOOL_VAL(false)); break;
//< Types of Values interpret-literals
//> Global Variables interpret-pop
      case OP_POP: pop(); break;
//< Global Variables interpret-pop
//> Local Variables interpret-get-local
      case OP_GET_LOCAL: {
        uint8_t slot = READ_BYTE();
/* Local Variables interpret-get-local < Calls and Functions push-local
        push(vm.stack[slot]); // [slot]
*/
//> Calls and Functions push-local
        push(frame->slots[slot]);
//< Calls and Functions push-local
        break;
      }
//< Local Variables interpret-get-local
//> Local Variables interpret-set-local
      case OP_SET_LOCAL: {
        uint8_t slot = READ_BYTE();
/* Local Variables interpret-set-local < Calls and Functions set-local
        vm.stack[slot] = peek(0);
*/
//> Calls and Functions set-local
        frame->slots[slot] = peek(0);
//< Calls and Functions set-local
        break;
      }
//< Local Variables interpret-set-local
//> Global Variables interpret-get-global
      case OP_GET_GLOBAL: {
        ObjString* name = READ_STRING();
        Value value;
        if (!tableGet(&vm.globals, name, &value)) {
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value);
        break;
      }
//< Global Variables interpret-get-global
//> Global Variables interpret-define-global
      case OP_DEFINE_GLOBAL: {
        ObjString* name = READ_STRING();
        tableSet(&vm.globals, name, peek(0));
        pop();
        break;
      }
//< Global Variables interpret-define-global
//> Global Variables interpret-set-global
      case OP_SET_GLOBAL: {
        ObjString* name = READ_STRING();
        if (tableSet(&vm.globals, name, peek(0))) {
          tableDelete(&vm.globals, name); // [delete]
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
//< Global Variables interpret-set-global
//> Closures interpret-get-upvalue
      case OP_GET_UPVALUE: {
        uint8_t slot = READ_BYTE();
        push(*frame->closure->upvalues[slot]->location);
        break;
      }
//< Closures interpret-get-upvalue
//> Closures interpret-set-upvalue
      case OP_SET_UPVALUE: {
        uint8_t slot = READ_BYTE();
        *frame->closure->upvalues[slot]->location = peek(0);
        break;
      }
//< Closures interpret-set-upvalue
//> Classes and Instances interpret-get-property
      case OP_GET_PROPERTY: {
//> get-not-instance
        if (!IS_INSTANCE(peek(0))) {
          runtimeError("Only instances have properties.");
          return INTERPRET_RUNTIME_ERROR;
        }

//< get-not-instance
        ObjInstance* instance = AS_INSTANCE(peek(0));
        ObjString* name = READ_STRING();
        
        Value value;
        if (tableGet(&instance->fields, name, &value)) {
          pop(); // Instance.
          push(value);
          break;
        }
//> get-undefined

//< get-undefined
/* Classes and Instances get-undefined < Methods and Initializers get-method
        runtimeError("Undefined property '%s'.", name->chars);
        return INTERPRET_RUNTIME_ERROR;
*/
//> Methods and Initializers get-method
        if (!bindMethod(instance->klass, name)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
//< Methods and Initializers get-method
      }
//< Classes and Instances interpret-get-property
//> Classes and Instances interpret-set-property
      case OP_SET_PROPERTY: {
//> set-not-instance
        if (!IS_INSTANCE(peek(1))) {
          runtimeError("Only instances have fields.");
          return INTERPRET_RUNTIME_ERROR;
        }

//< set-not-instance
        ObjInstance* instance = AS_INSTANCE(peek(1));
        tableSet(&instance->fields, READ_STRING(), peek(0));
        Value value = pop();
        pop();
        push(value);
        break;
      }
//< Classes and Instances interpret-set-property
//> Superclasses interpret-get-super
      case OP_GET_SUPER: {
        ObjString* name = READ_STRING();
        ObjClass* superclass = AS_CLASS(pop());
        
        if (!bindMethod(superclass, name)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
//< Superclasses interpret-get-super
//> Types of Values interpret-equal
      case OP_EQUAL: {
        Value b = pop();
        Value a = pop();
        push(BOOL_VAL(valuesEqual(a, b)));
        break;
      }
//< Types of Values interpret-equal
//> Types of Values interpret-comparison
      case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
      case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;
//< Types of Values interpret-comparison
/* A Virtual Machine op-binary < Types of Values op-arithmetic
      case OP_ADD:      BINARY_OP(+); break;
      case OP_SUBTRACT: BINARY_OP(-); break;
      case OP_MULTIPLY: BINARY_OP(*); break;
      case OP_DIVIDE:   BINARY_OP(/); break;
*/
/* A Virtual Machine op-negate < Types of Values op-negate
      case OP_NEGATE:   push(-pop()); break;
*/
/* Types of Values op-arithmetic < Strings add-strings
      case OP_ADD:      BINARY_OP(NUMBER_VAL, +); break;
*/
//> Strings add-strings
      case OP_ADD: {
        if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
          concatenate();
        } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
          double b = AS_NUMBER(pop());
          double a = AS_NUMBER(pop());
          push(NUMBER_VAL(a + b));
        } else {
          runtimeError(
              "Operands must be two numbers or two strings.");
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
//< Strings add-strings
//> Types of Values op-arithmetic
      case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
      case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
      case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
//< Types of Values op-arithmetic
//> Types of Values op-not
      case OP_NOT:
        push(BOOL_VAL(isFalsey(pop())));
        break;
//< Types of Values op-not
//> Types of Values op-negate
      case OP_NEGATE:
        if (!IS_NUMBER(peek(0))) {
          runtimeError("Operand must be a number.");
          return INTERPRET_RUNTIME_ERROR;
        }
        push(NUMBER_VAL(-AS_NUMBER(pop())));
        break;
//< Types of Values op-negate
//> Global Variables interpret-print
      case OP_PRINT: {
        printValue(pop());
        printf("\n");
        break;
      }
//< Global Variables interpret-print
//> Jumping Back and Forth op-jump
      case OP_JUMP: {
        uint16_t offset = READ_SHORT();
/* Jumping Back and Forth op-jump < Calls and Functions jump
        vm.ip += offset;
*/
//> Calls and Functions jump
        frame->ip += offset;
//< Calls and Functions jump
        break;
      }
//< Jumping Back and Forth op-jump
//> Jumping Back and Forth op-jump-if-false
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();
/* Jumping Back and Forth op-jump-if-false < Calls and Functions jump-if-false
        if (isFalsey(peek(0))) vm.ip += offset;
*/
//> Calls and Functions jump-if-false
        if (isFalsey(peek(0))) frame->ip += offset;
//< Calls and Functions jump-if-false
        break;
      }
//< Jumping Back and Forth op-jump-if-false
//> Jumping Back and Forth op-loop
      case OP_LOOP: {
        uint16_t offset = READ_SHORT();
/* Jumping Back and Forth op-loop < Calls and Functions loop
        vm.ip -= offset;
*/
//> Calls and Functions loop
        frame->ip -= offset;
//< Calls and Functions loop
        break;
      }
//< Jumping Back and Forth op-loop
//> Calls and Functions interpret-call
      case OP_CALL: {
        int argCount = READ_BYTE();
        if (!callValue(peek(argCount), argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
//> update-frame-after-call
        frame = &vm.frames[vm.frameCount - 1];
//< update-frame-after-call
        break;
      }
//< Calls and Functions interpret-call
//> Methods and Initializers interpret-invoke
      case OP_INVOKE: {
        ObjString* method = READ_STRING();
        int argCount = READ_BYTE();
        if (!invoke(method, argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
//< Methods and Initializers interpret-invoke
//> Superclasses interpret-super-invoke
      case OP_SUPER_INVOKE: {
        ObjString* method = READ_STRING();
        int argCount = READ_BYTE();
        ObjClass* superclass = AS_CLASS(pop());
        if (!invokeFromClass(superclass, method, argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
//< Superclasses interpret-super-invoke
//> Closures interpret-closure
      case OP_CLOSURE: {
        ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
        ObjClosure* closure = newClosure(function);
        push(OBJ_VAL(closure));
//> interpret-capture-upvalues
        for (int i = 0; i < closure->upvalueCount; i++) {
          uint8_t isLocal = READ_BYTE();
          uint8_t index = READ_BYTE();
          if (isLocal) {
            closure->upvalues[i] =
                captureUpvalue(frame->slots + index);
          } else {
            closure->upvalues[i] = frame->closure->upvalues[index];
          }
        }
//< interpret-capture-upvalues
        break;
      }
//< Closures interpret-closure
//> Closures interpret-close-upvalue
      case OP_CLOSE_UPVALUE:
        closeUpvalues(vm.stackTop - 1);
        pop();
        break;
//< Closures interpret-close-upvalue
      case OP_RETURN: {
/* A Virtual Machine print-return < Global Variables op-return
        printValue(pop());
        printf("\n");
*/
/* Global Variables op-return < Calls and Functions interpret-return
        // Exit interpreter.
*/
/* A Virtual Machine run < Calls and Functions interpret-return
        return INTERPRET_OK;
*/
//> Calls and Functions interpret-return
        Value result = pop();
//> Closures return-close-upvalues
        closeUpvalues(frame->slots);
//< Closures return-close-upvalues
        vm.frameCount--;
        if (vm.frameCount == 0) {
          pop();
          return INTERPRET_OK;
        }

        vm.stackTop = frame->slots;
        push(result);
        frame = &vm.frames[vm.frameCount - 1];
        break;
//< Calls and Functions interpret-return
      }
//> Classes and Instances interpret-class
      case OP_CLASS:
        push(OBJ_VAL(newClass(READ_STRING())));
        break;
//< Classes and Instances interpret-class
//> Superclasses interpret-inherit
      case OP_INHERIT: {
        Value superclass = peek(1);
//> inherit-non-class
        if (!IS_CLASS(superclass)) {
          runtimeError("Superclass must be a class.");
          return INTERPRET_RUNTIME_ERROR;
        }

//< inherit-non-class
        ObjClass* subclass = AS_CLASS(peek(0));
        tableAddAll(&AS_CLASS(superclass)->methods,
                    &subclass->methods);
        pop(); // Subclass.
        break;
      }
//< Superclasses interpret-inherit
//> Methods and Initializers interpret-method
      case OP_METHOD:
        defineMethod(READ_STRING());
        break;
//< Methods and Initializers interpret-method
    }
  }

#undef READ_BYTE
//> Jumping Back and Forth undef-read-short
#undef READ_SHORT
//< Jumping Back and Forth undef-read-short
//> undef-read-constant
#undef READ_CONSTANT
//< undef-read-constant
//> Global Variables undef-read-string
#undef READ_STRING
//< Global Variables undef-read-string
//> undef-binary-op
#undef BINARY_OP
//< undef-binary-op
}
//< run
//> omit
void hack(bool b) {
  // Hack to avoid unused function error. run() is not used in the
  // scanning chapter.
  run();
  if (b) hack(false);
}
//< omit
//> interpret
/* A Virtual Machine interpret < Scanning on Demand vm-interpret-c
InterpretResult interpret(Chunk* chunk) {
  vm.chunk = chunk;
  vm.ip = vm.chunk->code;
  return run();
*/
//> Scanning on Demand vm-interpret-c
InterpretResult interpret(const char* source) {
/* Scanning on Demand vm-interpret-c < Compiling Expressions interpret-chunk
  compile(source);
  return INTERPRET_OK;
*/
/* Compiling Expressions interpret-chunk < Calls and Functions interpret-stub
  Chunk chunk;
  initChunk(&chunk);

  if (!compile(source, &chunk)) {
    freeChunk(&chunk);
    return INTERPRET_COMPILE_ERROR;
  }

  vm.chunk = &chunk;
  vm.ip = vm.chunk->code;
*/
//> Calls and Functions interpret-stub
  ObjFunction* function = compile(source);
  if (function == NULL) return INTERPRET_COMPILE_ERROR;

  push(OBJ_VAL(function));
//< Calls and Functions interpret-stub
/* Calls and Functions interpret-stub < Calls and Functions interpret
  CallFrame* frame = &vm.frames[vm.frameCount++];
  frame->function = function;
  frame->ip = function->chunk.code;
  frame->slots = vm.stack;
*/
/* Calls and Functions interpret < Closures interpret
  call(function, 0);
*/
//> Closures interpret
  ObjClosure* closure = newClosure(function);
  pop();
  push(OBJ_VAL(closure));
  call(closure, 0);
//< Closures interpret
//< Scanning on Demand vm-interpret-c
//> Compiling Expressions interpret-chunk

/* Compiling Expressions interpret-chunk < Calls and Functions end-interpret
  InterpretResult result = run();

  freeChunk(&chunk);
  return result;
*/
//> Calls and Functions end-interpret
  return run();
//< Calls and Functions end-interpret
//< Compiling Expressions interpret-chunk
}
//< interpret
