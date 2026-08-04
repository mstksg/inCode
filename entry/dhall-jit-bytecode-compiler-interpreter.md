dhall-jit: A Bytecode Compiler and Interpreter for Dhall
========================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/dhall-jit-bytecode-compiler-interpreter.html)

## What this is

A bytecode compiler and multi-backend execution engine for Dhall. Compiles Dhall
expressions to a stack-based bytecode, then executes via a pure Haskell
interpreter or a C interpreter via FFI.

                    Dhall Expr
                   /          \
       compileExpr             normalize (dhall library)
                 /              \
                v                v
           ProgramBanks        Expr (NF)
                |
         +------+------+
         |             |
         v             v
      Haskell       C interp
      interp         (FFI)

The motivation: Dhall's built-in normalizer produces Dhall `Expr` values, which
means running a Dhall function requires re-normalizing the full expression tree
on every application. For functions called in hot loops (configuration functions
applied per-element), this is unacceptably slow. The bytecode compiler pays a
one-time cost to compile the function into a flat representation, then the
interpreter can execute repeated applications at near-native speed without
re-traversing syntax trees.

## The Bytecode

### ProgramBanks

The compiled representation is a map from integer procedure IDs to instruction
sequences:

``` haskell
type ProgramBanks = IntMap (NESeq (Instr Int))
```

`NESeq` (non-empty sequence) because every bank must have at least one
instruction. The `Int` parameter on `Instr` is the type of jump targets --
procedure IDs. Each bank has an implicit RETURN at the end (the final value left
on the stack is the return value).

-   Bank 0 is the entry point (MAIN)
-   Lambda bodies, if-branches, and let-bindings each get their own bank
-   Each bank is a straight-line instruction sequence (no jumps within a bank)
-   All branching is done by calling into other banks

### Execution model

Execution state per procedure call:

-   A **data stack** (list of `Value`) for intermediate values
-   A **read-only environment** (`Seq (Value Int)`) -- the captured closure
    variables plus the current argument
-   The **program banks** (immutable code store, shared across all calls)

When a procedure finishes executing all its instructions, it pops the single
remaining stack value as its return value.

### Why no jumps

We can avoid JUMPs completely by treating merges and if/then/else as function
calls into other banks. Every bank is a straight-line sequence with an implicit
RETURN at the end. This means:

-   No control-flow analysis needed
-   Each bank is independently compilable
-   The C interpreter maps trivially: one C function per bank
-   Serialization is trivial (just serialize each bank independently)

### Supporting types

``` haskell
-- Primitives: the leaf values of the Dhall type system
data Prim
    = PNat !Natural
    | PInt !Integer
    | PDouble !Double
    | PBool !Bool
    | PText !Text
    | PBytes !ByteString
    | PDate !Day
    | PTime !TimeOfDay
    | PTimeZone !TimeZone
    | PAssert
    | PType
    -- PType is a dummy value used to fill stack slots for type arguments.
    -- Dhall has dependent types at the kind level: (\(a : Type) -> \(x : a) -> x)
    -- needs *something* on the stack for the Type argument so de Bruijn indices
    -- line up. PType occupies that slot and is never inspected.

-- Path component for WITH instruction (nested record update)
data PathComponent
    = PathLabel !Word16  -- field index
    | PathQuestion       -- Optional's Some case

-- Arity of a union alternative (used by MERGE_UNION)
data UnionArity = Nullary | Unary

-- How to merge a field from two records
data RecordMergeOp a
    = FromLeft !a    -- take from left operand at index a
    | FromRight !a   -- take from right operand at index a

-- Builtin operations and functions
data DhallBuiltin
    = ADD_NAT | MUL_NAT | TEXT_APPEND | LIST_APPEND
    | AND_BOOL | OR_BOOL | EQ_BOOL | NE_BOOL
    | NATURAL_FOLD | NATURAL_BUILD
    | NATURAL_IS_ZERO | NATURAL_EVEN | NATURAL_ODD
    | NATURAL_TO_INTEGER | NATURAL_SHOW | NATURAL_SUBTRACT
    | INTEGER_CLAMP | INTEGER_NEGATE | INTEGER_SHOW | INTEGER_TO_DOUBLE
    | DOUBLE_SHOW | TEXT_SHOW | TEXT_REPLACE
    | DATE_SHOW | TIME_SHOW | TIMEZONE_SHOW
    | LIST_BUILD | LIST_FOLD
    | LIST_LENGTH | LIST_HEAD | LIST_LAST | LIST_INDEXED | LIST_REVERSE
    | NONE

-- Functions provided by the runtime to user code (for fold/build)
data ProvidedFn = NaturalSucc | ListCons
```

### The instruction set

``` haskell
data Instr i
    = LOAD_LOCAL !Word8
    -- Push env[i] onto the stack.
    -- Index 0 is the current argument (innermost lambda parameter).
    -- Index 1+ are captured variables from the enclosing scope.

    | MAKE_LIST !Word16
    -- Pop N values from the stack (topmost = last element),
    -- construct a VList. N=0 is valid (empty list).

    | MAKE_RECORD !Word16
    -- Pop N values from the stack (topmost = last field),
    -- construct a VRecord. Fields are in alphabetically-sorted order
    -- (matching Dhall's canonical field ordering).

    | LOAD_FIELD !Word16
    -- Pop a record, push the field at the given index.

    | MAKE_UNION !Word8
    -- Pop a value, wrap it as VUnion with the given tag.

    | MAKE_NULLARY_UNION !Word8
    -- Push VUnion tag Nothing (no pop -- the constructor has no payload).

    | MERGE_UNION !(Vector UnionArity)
    -- Pop a union value, then pop a handler record.
    -- The Vector gives the arity of each alternative (indexed by tag).
    -- If Nullary: push the handler directly.
    -- If Unary: CALL the handler with the union's payload.

    | MERGE_IF !i !i
    -- Pop a Bool. If True, execute bank i1; if False, execute bank i2.
    -- Both banks inherit the current environment.

    | PUSH_PRIM !Prim
    -- Push a literal value.

    | PUSH_BUILTIN !DhallBuiltin
    -- Push VBuiltin with zero captured arguments.

    | BINOP !DhallBuiltin
    -- Pop two values [left, right] (right on top), apply the binary
    -- operation, push the result. Only used for: ADD_NAT, MUL_NAT,
    -- TEXT_APPEND, LIST_APPEND, AND_BOOL, OR_BOOL, EQ_BOOL, NE_BOOL.

    | CLOSURE i
    -- Capture the entire current environment, push VLam i env.

    | CALL
    -- Pop arg (top), pop fn (second).
    -- If fn is VLam: execute bank with env = arg <| capturedEnv.
    -- If fn is VBuiltin: accumulate arg; fire if saturated.
    -- If fn is VProvided: accumulate arg; fire if saturated.

    | SHOW_CONSTRUCTOR !(Vector Text)
    -- Pop a union, push the constructor name (as PText) by indexing
    -- into the vector with the union's tag.

    | TOMAP !(Vector Text)
    -- Pop a record, produce a list of {mapKey: Text, mapValue: a}
    -- records. Vector contains field names in sorted order.

    | WITH !(NESeq PathComponent)
    -- Pop new value (top), pop base value (second).
    -- Traverse base along the path, replacing the final target with
    -- the new value. PathLabel descends into record fields; PathQuestion
    -- descends into Some.

    | PROJECT !(Vector Word16)
    -- Pop a record, push a new record containing only the fields at
    -- the given indices (in sorted order).

    | COMBINE !(Vector (RecordMergeOp (NESeq Word16)))
    -- Pop right record (top), pop left record (second).
    -- Each op says: for this output field, take from left or right at
    -- the given path. NESeq Word16 is a path of indices for recursive
    -- merging into nested records.

    | PREFER !(Vector (RecordMergeOp Word16))
    -- Pop right record (top), pop left record (second).
    -- Each op says: take field from left at idx, or from right at idx.
    -- Right wins on collision (Dhall's // semantics).
```

### Runtime values

``` haskell
data Value i
    = VPrim !Prim
    | VRecord !(Vector (Value i))
    | VList !(Vector (Value i))
    | VUnion !Word8 !(Maybe (Value i))
    | VLam !i (Seq (Value i))
    | VBuiltin !DhallBuiltin [Value i]
    | VProvided !ProvidedFn [Value i]
```

Key difference from a normalized Dhall Expr: no `App` (everything is
fully-applied), and lambdas are jumps with closures rather than syntax trees.

`VBuiltin` and `VProvided` accumulate arguments in a list. When a `CALL`
provides an argument to a `VBuiltin`, it appends the arg. Once the arg count
reaches the builtin's arity, the builtin executes and produces a result value.
This means partially-applied builtins are first-class values -- `Natural/add 3`
is `VBuiltin ADD_NAT [VPrim (PNat 3)]` sitting on the stack, waiting for one
more arg.

`VProvided` exists for the two functions that `Natural/fold` and `List/build`
pass to user code: `NaturalSucc` (increments a Natural) and `ListCons` (prepends
to a list). These are runtime-synthesized functions, not user-written lambdas.

## The Compiler (Compile.hs)

The compiler works in a single pass over a type-checked, alpha-normalized Dhall
expression. It emits instructions into the current bank, allocating new banks
for sub-expressions that need their own scope.

### Compiler state

``` haskell
newtype CompileState = CS {csBanks :: ProgramBanks}

data CompileContext s = CC
    { ccDhallCtx :: !(Context (FunctionBinding s Void))
    -- ^ Dhall typing context: tracks what variables are in scope and their types.
    --   Used for type-directed compilation (field lookups, union arities).
    , ccCurrBank :: !Int
    -- ^ Which bank we are currently emitting instructions into.
    }

type Compile s = ReaderT (CompileContext s) (StateT CompileState (Except (CompileError s)))
```

### Bank allocation

``` haskell
-- Allocate a new bank ID (one past the current max)
allocBank :: Compile s Int

-- Run an action emitting into a fresh bank, return the bank ID
withNewBank_ :: Compile s a -> Compile s Int
```

### How each Dhall construct compiles

**Variables** (`Var (V _ idx)`):

    emit $ LOAD_LOCAL (fromIntegral idx)

De Bruijn index directly becomes the environment offset.

**Lambdas** (`Lam _ binding body`):

    bodyBank <- withNewBank_ $ compileExpr_ body  -- body goes into its own bank
    emit $ CLOSURE bodyBank                        -- capture current env

The lambda body is compiled into a separate bank. At runtime, `CLOSURE bodyBank`
captures the current environment and pushes a `VLam bodyBank env`. When this
closure is later `CALL`ed, the interpreter enters `bodyBank` with `arg <| env`
as the new environment.

**Application** (`App f x`):

    compileExpr_ f          -- push the function
    compileExpr_ x          -- push the argument (or PType if x is a Type)
    emit CALL               -- invoke

Type arguments are detected at compile time: if the argument's type is
`Const Type/Kind/Sort`, we push `PType` instead of compiling the actual type
expression (it will never be inspected at runtime).

**Let bindings** (`Let binding body`):

    compileExpr_ (value binding)   -- push the bound value
    bodyBank <- withNewBank_ $ compileExpr_ body
    emit $ CLOSURE bodyBank        -- body as a lambda
    emit CALL                      -- immediately apply

`let x = e1 in e2` is semantically `(\x -> e2) e1`. The compiler emits it
literally that way. The body bank gets the bound value as env\[0\].

**If/then/else** (`BoolIf b t f`):

    compileExpr_ b
    tBank <- withNewBank_ $ compileExpr_ t
    fBank <- withNewBank_ $ compileExpr_ f
    emit $ MERGE_IF tBank fBank

Both branches are compiled into separate banks. At runtime, the interpreter pops
the Bool and calls into the chosen bank (inheriting the current env).

**Record literals** (`RecordLit fields`):

    traverse_ (compileExpr_ . recordFieldValue . snd) (DM.toAscList fields)
    emit $ MAKE_RECORD (fromIntegral (DM.size fields))

Fields are compiled in alphabetical order (Dhall's canonical ordering). The
record is positional from this point on.

**Field access** (`Field record fieldName`):

    -- At compile time: look up the field's index in the record type
    recordType <- typeOf record
    idx <- fieldIndex recordType fieldName
    compileExpr_ record
    emit $ LOAD_FIELD (fromIntegral idx)

The type checker tells us what position the field occupies. No string comparison
at runtime.

**Union constructors** (`Field (Union alts) name`):

For a nullary constructor:

    emit $ MAKE_NULLARY_UNION (fromIntegral idx)

For a unary constructor (takes a payload):

    lambdaBank <- withNewBank_ $ do
        emit $ LOAD_LOCAL 0          -- the payload
        emit $ MAKE_UNION (fromIntegral idx)
    emit $ CLOSURE lambdaBank        -- push the constructor as a function

A unary union constructor compiles to a lambda that wraps its argument.

**Merge** (`Merge handlers union _`):

    unionAlts <- typeOf union  -- get the union type to know arities
    compileExpr_ handlers
    compileExpr_ union
    emit $ MERGE_UNION $ V.fromList $ altArity <$> DM.toAscList unionAlts

The arity vector tells the interpreter whether each alternative is Nullary (just
return the handler) or Unary (call the handler with the payload).

**Prefer** (`Prefer _ _ l r`):

    lFields <- getRecordFields l
    rFields <- getRecordFields r
    compileExpr_ l
    compileExpr_ r
    emit $ PREFER $ V.fromList $ buildPreferOps lFields rFields

`buildPreferOps` produces a vector of `FromLeft idx | FromRight idx` for each
output field. On collision, right wins. The interpreter just iterates the vector
and indexes into the appropriate operand.

**Combine** (`Combine _ _ l r`):

    lFields <- getRecordFields l
    rFields <- getRecordFields r
    compileExpr_ l
    compileExpr_ r
    emit $ COMBINE $ V.fromList $ buildCombineOps lFields rFields

Like PREFER, but recursive. When both sides have a record at the same field,
`buildCombineOps` encodes the full path as a `NESeq Word16`. The interpreter
follows the path to recursively merge nested records.

**Text interpolation** (`TextLit (Chunks pairs suffix)`):

    -- "Hello, ${name}!" compiles as:
    -- PUSH_PRIM "Hello, "
    -- [compile name]
    -- BINOP TEXT_APPEND
    -- PUSH_PRIM "!"
    -- BINOP TEXT_APPEND

A chain of TEXT_APPEND operations. Each interpolated chunk pushes a prefix
string, compiles the interpolation, and appends.

**Builtins** (e.g. `NaturalFold`, `ListBuild`, etc.):

    emit $ PUSH_BUILTIN NATURAL_FOLD

Builtins are values. They sit on the stack as `VBuiltin name []` until enough
arguments accumulate via `CALL`.

**Binary operators** (`NaturalPlus l r`, `BoolAnd l r`, etc.):

    compileExpr_ l
    compileExpr_ r
    emit $ BINOP ADD_NAT   -- (or appropriate op)

## The Haskell Interpreter (Interpret.hs)

### Execution monad

``` haskell
data ExecContext = ExecContext
    { ecBanks :: !ProgramBanks
    , ecEnv   :: !(Seq (Value Int))
    }

type Exec s = ReaderT ExecContext (Except (ExecError s))
```

### Core execution loop

``` haskell
executeProcedure :: Int -> Seq (Value Int) -> Exec s (Value Int)
executeProcedure procId newEnv = do
    instrs <- lookupBank procId
    local (\c -> c{ecEnv = newEnv}) $
        evalStateT (traverse_ executeInstr (toList instrs) >> pop) []
```

Enter a bank, set the environment, execute all instructions (they push/pop from
the stack), then pop the final value as the result.

### Instruction execution (key cases)

**CALL** -- the heart of the interpreter:

``` haskell
CALL -> do
    arg <- pop
    fn <- pop
    push =<< case fn of
        VLam procId closedEnv ->
            lift $ executeProcedure procId (arg <| closedEnv)
        VBuiltin builtin capturedArgs ->
            lift $ applyBuiltin builtin (arg :| capturedArgs)
        VProvided provFn capturedArgs ->
            lift $ executeProvidedFn provFn (arg :| capturedArgs)
        _ -> throwError $ ExpectedFunction fn
```

For `VLam`: recursively execute the target bank with `arg` prepended to the
captured env.

For `VBuiltin`: add the arg to the list; if now saturated, fire the builtin.

**MERGE_UNION**:

``` haskell
MERGE_UNION arities -> do
    union <- pop
    handlers <- pop
    case (union, handlers) of
        (VUnion tag payload, VRecord handlerFields) ->
            case (arities V.!? fromIntegral tag, handlerFields V.!? fromIntegral tag, payload) of
                (Just Nullary, Just handler, Nothing) -> push handler
                (Just Unary, Just handler, Just payloadVal) -> do
                    -- Apply handler to payload
                    push handler
                    push payloadVal
                    executeInstr CALL
```

**MERGE_IF**:

``` haskell
MERGE_IF tProc fProc -> do
    cond <- pop
    push =<< case cond of
        VPrim (PBool True)  -> lift $ executeProcedure tProc env
        VPrim (PBool False) -> lift $ executeProcedure fProc env
```

### Builtin application

Builtins fire when all arguments have arrived. Key cases:

``` haskell
applyBuiltin :: DhallBuiltin -> NonEmpty (Value Int) -> Exec s (Value Int)
applyBuiltin builtin allArgs = case (builtin, allArgs) of
    -- Arity 1
    (NATURAL_IS_ZERO, VPrim (PNat n) :| []) ->
        pure $ VPrim $ PBool (n == 0)
    (NATURAL_TO_INTEGER, VPrim (PNat n) :| []) ->
        pure $ VPrim $ PInt (fromIntegral n)

    -- Arity 2 (note: args accumulate in order, so last applied is head)
    (NATURAL_SUBTRACT, VPrim (PNat y) :| [VPrim (PNat x)]) ->
        pure $ VPrim $ PNat (if y >= x then y - x else 0)
    (LIST_LENGTH, VList xs :| [_]) ->
        pure $ VPrim $ PNat (fromIntegral (V.length xs))
    (LIST_HEAD, VList xs :| [_]) -> case V.uncons xs of
        Just (x, _) -> pure $ VUnion 1 (Just x)   -- Some x
        Nothing     -> pure $ VUnion 0 Nothing     -- None

    -- Arity 3
    (TEXT_REPLACE, VPrim (PText haystack) :| [VPrim (PText replacement), VPrim (PText needle)]) ->
        pure $ VPrim $ PText (T.replace needle replacement haystack)

    -- Natural/fold: the interesting one
    (NATURAL_FOLD, initVal :| [succ', _, VPrim (PNat n)]) ->
        applyNTimes succ' initVal n

    -- List/fold: fold over a list
    (LIST_FOLD, initVal :| [cons, _, VList xs, _]) ->
        V.foldr (\x acc -> applyTwice cons x =<< acc) (pure initVal) xs

    -- Not saturated yet: accumulate
    (builtin', args) -> pure $ VBuiltin builtin' (toList args)
```

**Natural/fold** applies the successor function N times:

``` haskell
applyNTimes _ acc 0 = pure acc
applyNTimes (VLam procId closedEnv) acc n = do
    result <- executeProcedure procId (acc <| closedEnv)
    applyNTimes (VLam procId closedEnv) result (n - 1)
```

**List/fold** applies `cons x acc` for each element:

``` haskell
applyTwice (VLam procId1 env1) x acc = do
    partial <- executeProcedure procId1 (x <| env1)    -- cons x
    case partial of
        VLam procId2 env2 -> executeProcedure procId2 (acc <| env2)  -- (cons x) acc
```

**Natural/build** and **List/build** pass runtime-provided functions to user
code:

``` haskell
(NATURAL_BUILD, VLam procId closedEnv :| []) -> do
    -- build f = f Natural Natural/succ 0
    -- We skip the Type argument, pass succ and zero:
    partial <- executeProcedure procId (VProvided NaturalSucc [] <| closedEnv)
    case partial of
        VLam procId' closedEnv' ->
            executeProcedure procId' (VPrim (PNat 0) <| closedEnv')
```

The `VProvided NaturalSucc` is handed to user code. When user code calls it (via
`CALL`), `executeProvidedFn` handles it:

``` haskell
executeProvidedFn :: ProvidedFn -> NonEmpty (Value Int) -> Exec s (Value Int)
executeProvidedFn provFn allArgs = case (provFn, allArgs) of
    (NaturalSucc, VPrim (PNat n) :| []) -> pure $ VPrim $ PNat (n + 1)
    (ListCons, x :| [VList xs])         -> pure $ VList (V.cons x xs)
    (provFn', args)                      -> pure $ VProvided provFn' (toList args)
```

### The RunResult protocol

``` haskell
data RunResult s r a
    = RunFailure !(ExecError s)
    | RunClosure (r -> RunResult s r a)
    | RunSuccess !a
```

When executing a program, if the result is a `VLam`, it becomes `RunClosure` --
a Haskell function waiting for the next argument. This is the interface for
calling compiled Dhall functions from Haskell incrementally:

``` haskell
valueToRunResult :: ProgramBanks -> Value Int -> RunResult s EValue EValue
valueToRunResult banks (VLam procId closedEnv) = RunClosure $ \arg ->
    case evalueToValue arg of
        Right argVal -> case runExec (executeProcedure procId (argVal <| closedEnv)) of
            Right result -> valueToRunResult banks result
            Left err -> RunFailure err
valueToRunResult _ (VPrim p) = RunSuccess $ EPrim p
-- ... (records, lists, unions convert recursively)
```

`EValue` is the closure-free subset of `Value` -- the external interface type:

``` haskell
data EValue
    = EPrim !Prim
    | ERecord !(Vector EValue)
    | EList !(Vector EValue)
    | EUnion !Word8 !(Maybe EValue)
    | EBuiltin !DhallBuiltin [EValue]
    | EProvided !ProvidedFn [EValue]
```

## The C Interpreter (dhall_interpreter.c)

A direct translation of the Haskell interpreter into C, operating over the same
bytecode serialized into C structs.

### Data layout

``` c
typedef enum {
  PRIM_NAT, PRIM_INT, PRIM_DOUBLE, PRIM_BOOL, PRIM_TEXT,
  PRIM_BYTES, PRIM_DATE, PRIM_TIME, PRIM_TIMEZONE, PRIM_ASSERT, PRIM_TYPE
} PrimTag;

typedef struct {
  PrimTag tag;
  union {
    uint64_t nat;
    int64_t int_val;
    double double_val;
    bool bool_val;
    char *text;
    struct { uint8_t *data; size_t len; } bytes;
    int32_t date;      // days since epoch
    int64_t time;      // nanoseconds since midnight
    int32_t timezone;  // offset in minutes
  } data;
} Prim;

typedef enum {
  VAL_PRIM, VAL_RECORD, VAL_LIST, VAL_UNION,
  VAL_LAM, VAL_BUILTIN, VAL_PROVIDED
} ValueTag;

struct Value {
  ValueTag tag;
  union {
    Prim prim;
    struct { Value **fields; size_t len; } record;
    struct { Value **elems; size_t len; } list;
    struct { uint8_t tag; Value *payload; } union_val;
    struct { int proc_id; Value **env; size_t env_len; } lam;
    struct { DhallBuiltin builtin; Value **args; size_t args_len; } builtin;
    struct { ProvidedFn fn; Value **args; size_t args_len; } provided;
  } data;
};
```

### Program structure

``` c
typedef struct {
  uint8_t tag;  // InstrTag
  union {
    uint8_t load_local_idx;
    uint16_t make_list_n;
    uint16_t make_record_n;
    uint16_t load_field_idx;
    uint8_t make_union_tag;
    uint8_t make_nullary_union_tag;
    struct { UnionArity *arities; size_t arities_len; } merge_union;
    struct { int true_proc; int false_proc; } merge_if;
    Prim push_prim;
    DhallBuiltin push_builtin;
    DhallBuiltin binop;
    int closure_proc;
    struct { char **names; size_t names_len; } show_constructor;
    struct { char **names; size_t names_len; } tomap;
    struct { PathComponent *path; size_t path_len; } with;
    struct { uint16_t *indices; size_t indices_len; } project;
    struct { RecordMergeOpSeq *ops; size_t ops_len; } combine;
    struct { RecordMergeOp *ops; size_t ops_len; } prefer;
  } data;
} Instr;

typedef struct { Instr *instrs; size_t len; } Procedure;
typedef struct { Procedure *procs; size_t num_procs; } Program;
```

### Execution context

``` c
#define MAX_STACK_SIZE 1024

struct ExecutionContext {
  Value *stack[MAX_STACK_SIZE];
  size_t stack_len;
  Value **env;
  size_t env_len;
  Program *program;
};
```

Fixed-size stack array. The interpreter pushes and pops pointers to heap-
allocated Values.

### Core execution

``` c
Value *execute_procedure(ExecutionContext *ctx, int proc_id,
                         Value **env, size_t env_len, ErrorCode *err) {
    // Save/restore env around the call
    Value **old_env = ctx->env;
    size_t old_env_len = ctx->env_len;
    ctx->env = env;
    ctx->env_len = env_len;

    Procedure *proc = &ctx->program->procs[proc_id];
    for (size_t i = 0; i < proc->len && *err == ERR_NONE; i++) {
        execute_instr(ctx, &proc->instrs[i], err);
    }

    ctx->env = old_env;
    ctx->env_len = old_env_len;
    return pop(ctx, err);
}
```

Each instruction case in `execute_instr` mirrors the Haskell interpreter exactly
-- CLOSURE allocates a Value with tag VAL_LAM and memcpy's the env, CALL checks
the function tag and either recurses into execute_procedure (for closures) or
accumulates args (for builtins), etc.

### CALL in C

``` c
case INSTR_CALL: {
    Value *arg = pop(ctx, err);
    Value *func = pop(ctx, err);

    if (func->tag == VAL_LAM) {
        size_t new_env_len = func->data.lam.env_len + 1;
        Value **new_env = malloc(sizeof(Value *) * new_env_len);
        new_env[0] = arg;
        memcpy(new_env + 1, func->data.lam.env,
               func->data.lam.env_len * sizeof(Value *));
        Value *result = execute_procedure(ctx, func->data.lam.proc_id,
                                          new_env, new_env_len, err);
        push(ctx, result, err);
    } else if (func->tag == VAL_BUILTIN) {
        // Accumulate arg; if saturated, fire builtin
        ...
    }
    break;
}
```

### The FFI boundary

Haskell serializes `ProgramBanks` to a binary format, passes it across FFI as
bytes, C deserializes into a `Program` struct, executes, and returns an
`ExecutionResult`:

``` c
typedef enum { RESULT_SUCCESS, RESULT_CLOSURE, RESULT_FAILURE } ResultTag;

typedef struct {
  ResultTag tag;
  union {
    Value *success;
    struct { int proc_id; Value **captured_env; size_t env_len; } closure;
    struct { ErrorCode code; char *message; } error;
  } data;
} ExecutionResult;
```

If the result is `RESULT_CLOSURE`, the Haskell side holds onto the proc_id and
captured_env, and can feed more arguments in subsequent FFI calls. This is the
progressive application protocol -- the same as `RunClosure` on the Haskell
side.

## The Native Backend (Compiled C via GCC)

The native backend translates bytecode directly to C source, compiles it with
GCC -O2, and dlopens the resulting `.so`. Each procedure bank becomes a static C
function; inter-bank calls go through a `dispatch` switch.

Given this Dhall expression:

``` dhall
\(xs : List Natural) ->
  List/fold Natural xs Natural
    (\(x : Natural) -> \(acc : Natural) -> x + acc) 0
```

The native backend emits (literal output of `dhall-jit-print --csource`):

``` c
#include <dhall_native_rt.h>

static Value *proc_0(Arena *a, Value **env, size_t env_len);
static Value *proc_1(Arena *a, Value **env, size_t env_len);
static Value *proc_2(Arena *a, Value **env, size_t env_len);
static Value *proc_3(Arena *a, Value **env, size_t env_len);

static Value *proc_0(Arena *a, Value **env, size_t env_len) {
    Value *stack[1024];
    int sp = 0;
    {
        Value *lam = value_alloc(a);
        lam->tag = VAL_LAM;
        lam->data.lam.proc_id = 1;
        lam->data.lam.env_len = env_len;
        if (env_len > 0) {
            lam->data.lam.env = arena_alloc(a, env_len * sizeof(Value*));
            memcpy(lam->data.lam.env, env, env_len * sizeof(Value*));
        } else {
            lam->data.lam.env = NULL;
        }
        stack[sp++] = lam;
    }
    return stack[sp - 1];
}

static Value *proc_1(Arena *a, Value **env, size_t env_len) {
    Value *stack[1024];
    int sp = 0;
    stack[sp++] = env[0];
    {
        Value *lam = value_alloc(a);
        lam->tag = VAL_LAM;
        lam->data.lam.proc_id = 2;
        lam->data.lam.env_len = env_len;
        if (env_len > 0) {
            lam->data.lam.env = arena_alloc(a, env_len * sizeof(Value*));
            memcpy(lam->data.lam.env, env, env_len * sizeof(Value*));
        } else {
            lam->data.lam.env = NULL;
        }
        stack[sp++] = lam;
    }
    stack[sp++] = make_prim_nat_ui(a, 0UL);
    { Value *init_v=stack[--sp]; Value *cons_v=stack[--sp]; Value *list_v=stack[--sp];
      size_t len=list_v->data.list.len;
      Value *acc=init_v;
      for(size_t i=len;i>0;i--){
        Value *elem=list_v->data.list.elems[i-1];
        Value *partial=call_value(a, cons_v, elem);
        acc=call_value(a, partial, acc);
      }
      stack[sp++]=acc; }
    return stack[sp - 1];
}

static Value *proc_2(Arena *a, Value **env, size_t env_len) {
    Value *stack[1024];
    int sp = 0;
    {
        Value *lam = value_alloc(a);
        lam->tag = VAL_LAM;
        lam->data.lam.proc_id = 3;
        lam->data.lam.env_len = env_len;
        if (env_len > 0) {
            lam->data.lam.env = arena_alloc(a, env_len * sizeof(Value*));
            memcpy(lam->data.lam.env, env, env_len * sizeof(Value*));
        } else {
            lam->data.lam.env = NULL;
        }
        stack[sp++] = lam;
    }
    return stack[sp - 1];
}

static Value *proc_3(Arena *a, Value **env, size_t env_len) {
    Value *stack[1024];
    int sp = 0;
    stack[sp++] = env[1];
    stack[sp++] = env[0];
    { Value *b2=stack[--sp]; Value *a2=stack[--sp];
      stack[sp++]=op_add_nat(a, &a2->data.prim.data.nat, &b2->data.prim.data.nat); }
    return stack[sp - 1];
}

Value *dispatch(Arena *a, int proc_id, Value **env, size_t env_len) {
    switch (proc_id) {
        case 0: return proc_0(a, env, env_len);
        case 1: return proc_1(a, env, env_len);
        case 2: return proc_2(a, env, env_len);
        case 3: return proc_3(a, env, env_len);
        default: abort();
    }
}
```

GCC inlines all four `proc_N` functions into `dispatch`, and inlines
`op_add_nat` within the proc_id=3 case. The fold loop calls `call_value`
(external symbol -- closure dispatch), but the addition hot path is branch-free
for small nats. `op_add_nat` uses a two-branch BigNat: values fitting in
`uint64_t` take a fast `__builtin_add_overflow` path, falling through to GMP
only on overflow.

### Why compile to C rather than LLVM or direct machine code

-   GCC's optimizer is free and excellent -- we get constant folding, inlining,
    register allocation, and instruction scheduling without implementing any of
    it
-   The generated C is trivial to read and debug
-   Cross-platform by construction (GCC targets everything)
-   The compilation overhead is large (\~600ms-3s for GCC) but paid once; the
    `.so` is cached on disk keyed by a hash of the bytecode

### Arena allocation

The native backend uses arena allocation: all Values created during one
execution are bump-allocated from a single arena, freed in one shot when the
call returns. This eliminates per-value malloc/free overhead and avoids the need
for a garbage collector.

``` c
// All allocation during execution goes through the arena
Value *value_alloc(Arena *a);
void  *arena_alloc(Arena *a, size_t bytes);
// At call boundary: create arena, execute, extract result, free arena
```

## Design decisions

**Flat closures over linked environments**: every `CLOSURE` captures the full
current env as a vector. Wasteful in space (captures variables that may never be
used) but simple and cache-friendly. No linked-list traversal on variable
access, just `env[idx]`. The waste is bounded because Dhall functions tend to be
small.

**No jumps, only banks**: makes the instruction sequence trivially serializable,
trivially parallelizable per-bank, and trivially translatable to C functions.
The cost is extra procedure-call overhead for if/else, but Dhall programs are
not control-flow-heavy.

**Type erasure at compile time**: record field names, union constructor names,
and type arguments are all resolved to positional indices by the compiler. The
bytecode and interpreter never see strings for field access. This is where the
compile-time type information pays off -- we can use the Dhall type checker as a
compilation oracle.

**COMBINE/PREFER as pre-computed merge plans**: rather than interpreting record
merges dynamically (which requires knowing field names), the compiler
pre-computes a vector of "take field idx from left/right" operations. The
interpreter just follows the plan. `COMBINE` handles recursive merging by
encoding the full path as a `NESeq Word16` -- e.g. `FromLeft (0 :<|| [2])` means
"output field comes from left operand, field 0, then sub-field 2."

**Builtins as partially-applied values**: `PUSH_BUILTIN` pushes a `VBuiltin`
with an empty arg list. Each `CALL` adds one arg. When the arg count hits the
builtin's arity, it fires. This means builtins don't need special syntax --
they're just values that happen to execute when saturated. Partial application
is free (it's just accumulating into a list).

**PType for de Bruijn alignment**: Dhall's type-level lambdas mean that
`\(a : Type) -> \(x : a) -> x` has two parameters. At runtime, `a` is never
inspected, but we still need it to occupy env\[0\] so that `x` is at env\[1\]
(since the compiler uses de Bruijn indices). `PType` is the placeholder.

**Let as immediate closure application**: this might seem wasteful (allocating a
closure just to immediately call it), but it means the compiler doesn't need a
separate mechanism for let-bindings. The interpreter's `CALL` path handles
everything uniformly. In practice the overhead is negligible because the
"closure" is just the current env pointer (already allocated).

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

