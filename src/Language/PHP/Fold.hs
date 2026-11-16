{-# LANGUAGE RankNTypes #-}

module Language.PHP.Fold
  ( -- * Annotation stripping and mapping
    stripAnnotations
  , mapAnnotation
    -- * Folds and Catamorphisms
  , foldExpr
  , foldStmtExprs
  , foldStmt
    -- * Queries (Monoidal accumulation)
  , queryExpr
  , queryStmt
  , allExprs
  , allVariables
    -- * Transformations (Bottom-up and Top-down)
  , transformExpr
  , transformStmt
  ) where

import Language.PHP.AST
import Data.Text (Text)
import Data.Monoid (Endo (..))

-- | Strip annotations from any Functor AST node (replacing them with @()@).
stripAnnotations :: Functor f => f a -> f ()
stripAnnotations = fmap (const ())

-- | Map a function over annotations in an AST node.
mapAnnotation :: Functor f => (a -> b) -> f a -> f b
mapAnnotation = fmap

-- | Bottom-up transformation of expressions.
--
-- Traverses expressions recursively. Closure use-clause variable bindings
-- (@function () use ($var, &$ref) { ... }@) capture variables from the
-- enclosing scope and are rewritten in lockstep with body occurrences when
-- transformed via 'transformExpr', preserving by-reference flags.
--
-- Parameter bindings (in functions, methods, closures, and arrow functions)
-- define new formal parameters rather than referencing outer scope variables;
-- parameter declarations ('Param') are not rewritten by 'transformExpr' or
-- reported as variable occurrences by 'queryExpr' (though expressions inside
-- parameter attributes and parameter default values are traversed). Scope-aware
-- alpha-renaming of formal parameters is intentionally beyond the scope of
-- this syntax-directed fold.
--
-- Subexpressions embedded within interpolated strings, heredocs and backtick
-- commands ('LitInterpolated', 'LitHeredocInterpolated', 'ExprShellExec') are
-- recursively rewritten.
transformExpr :: (Expr a -> Expr a) -> Expr a -> Expr a
transformExpr f = f . \case
  ExprVar a v -> case v of
    DynamicVar va e -> ExprVar a (DynamicVar va (transformExpr f e))
    SimpleVar {} -> ExprVar a v
  ExprLit a l -> case l of
    LitInterpolated la parts ->
      ExprLit a (LitInterpolated la (map (transformStringPart f) parts))
    LitHeredocInterpolated la tag parts ->
      ExprLit a (LitHeredocInterpolated la tag (map (transformStringPart f) parts))
    _ -> ExprLit a l
  ExprBinary a op e1 e2 -> ExprBinary a op (transformExpr f e1) (transformExpr f e2)
  ExprUnary a op e -> ExprUnary a op (transformExpr f e)
  ExprAssign a mOp e1 e2 -> ExprAssign a mOp (transformExpr f e1) (transformExpr f e2)
  ExprAssignRef a e1 e2 -> ExprAssignRef a (transformExpr f e1) (transformExpr f e2)
  ExprTernary a cond tExpr fExpr ->
    ExprTernary a (transformExpr f cond) (fmap (transformExpr f) tExpr) (transformExpr f fExpr)
  ExprNullCoalesce a e1 e2 -> ExprNullCoalesce a (transformExpr f e1) (transformExpr f e2)
  ExprClone a e mWith ->
    ExprClone a (transformExpr f e) (fmap (transformExpr f) mWith)
  ExprNew a target args ->
    let target' = case target of
          ClassTargetExpr e -> ClassTargetExpr (transformExpr f e)
          t -> t
        args' = map (transformArg f) args
    in ExprNew a target' args'
  ExprNewAnonClass a attrs modif args ext impl members ->
    let attrs' = map (transformAttributeGroup f) attrs
        args' = map (transformArg f) args
        members' = map (transformClassMember f) members
    in ExprNewAnonClass a attrs' modif args' ext impl members'
  ExprCall a fn args ->
    let fn' = transformExpr f fn
        args' = case args of
          ArgsList as -> ArgsList (map (transformArg f) as)
          FirstClassCallable -> FirstClassCallable
    in ExprCall a fn' args'
  ExprMethodCall a obj member args ->
    let obj' = transformExpr f obj
        member' = case member of
          MemberExpr e -> MemberExpr (transformExpr f e)
          m -> m
        args' = case args of
          ArgsList as -> ArgsList (map (transformArg f) as)
          FirstClassCallable -> FirstClassCallable
    in ExprMethodCall a obj' member' args'
  ExprNullsafeMethodCall a obj member args ->
    let obj' = transformExpr f obj
        member' = case member of
          MemberExpr e -> MemberExpr (transformExpr f e)
          m -> m
        args' = case args of
          ArgsList as -> ArgsList (map (\arg -> arg { argExpr = transformExpr f (argExpr arg) }) as)
          FirstClassCallable -> FirstClassCallable
    in ExprNullsafeMethodCall a obj' member' args'
  ExprPropertyFetch a obj member ->
    let obj' = transformExpr f obj
        member' = case member of
          MemberExpr e -> MemberExpr (transformExpr f e)
          m -> m
    in ExprPropertyFetch a obj' member'
  ExprNullsafePropertyFetch a obj member ->
    let obj' = transformExpr f obj
        member' = case member of
          MemberExpr e -> MemberExpr (transformExpr f e)
          m -> m
    in ExprNullsafePropertyFetch a obj' member'
  ExprStaticCall a target member args ->
    let target' = case target of
          ClassTargetExpr e -> ClassTargetExpr (transformExpr f e)
          t -> t
        member' = case member of
          MemberExpr e -> MemberExpr (transformExpr f e)
          m -> m
        args' = case args of
          ArgsList as -> ArgsList (map (transformArg f) as)
          FirstClassCallable -> FirstClassCallable
    in ExprStaticCall a target' member' args'
  ExprStaticPropertyFetch a target var ->
    let target' = case target of
          ClassTargetExpr e -> ClassTargetExpr (transformExpr f e)
          t -> t
    in ExprStaticPropertyFetch a target' var
  ExprClassConstFetch a target constName ->
    let target' = case target of
          ClassTargetExpr e -> ClassTargetExpr (transformExpr f e)
          t -> t
        constName' = case constName of
          ConstNameDynamic e -> ConstNameDynamic (transformExpr f e)
          c -> c
    in ExprClassConstFetch a target' constName'
  ExprArray a items ->
    let items' = map (transformArrayItem f) items
    in ExprArray a items'
  ExprList a items ->
    let items' = map (transformArrayItem f) items
    in ExprList a items'
  ExprArrayAccess a arr mIdx ->
    ExprArrayAccess a (transformExpr f arr) (fmap (transformExpr f) mIdx)
  ExprMatch a subject arms ->
    let subject' = transformExpr f subject
        arms' = map (\case
          MatchArm ann conds res -> MatchArm ann (map (transformExpr f) conds) (transformExpr f res)
          MatchDefault ann res -> MatchDefault ann (transformExpr f res)) arms
    in ExprMatch a subject' arms'
  ExprClosure a attrs byRef isStatic params uses retType stmts ->
    let attrs' = map (transformAttributeGroup f) attrs
        params' = map (transformParam f) params
        uses' = map (\(vn@(VarName va _), r) ->
          case f (ExprVar va (SimpleVar va vn)) of
            ExprVar _ (SimpleVar _ newVn) -> (newVn, r)
            _                             -> (vn, r)) uses
        stmts' = map (transformStmt f) stmts
    in ExprClosure a attrs' byRef isStatic params' uses' retType stmts'
  ExprArrowFunction a attrs byRef isStatic params retType expr ->
    let attrs' = map (transformAttributeGroup f) attrs
        params' = map (transformParam f) params
    in ExprArrowFunction a attrs' byRef isStatic params' retType (transformExpr f expr)
  ExprYield a mK mV ->
    ExprYield a (fmap (transformExpr f) mK) (fmap (transformExpr f) mV)
  ExprYieldFrom a e -> ExprYieldFrom a (transformExpr f e)
  ExprCast a cast e -> ExprCast a cast (transformExpr f e)
  ExprIsset a es -> ExprIsset a (map (transformExpr f) es)
  ExprEmpty a e -> ExprEmpty a (transformExpr f e)
  ExprEval a e -> ExprEval a (transformExpr f e)
  ExprInclude a inc e -> ExprInclude a inc (transformExpr f e)
  ExprPrint a e -> ExprPrint a (transformExpr f e)
  ExprShellExec a parts -> ExprShellExec a (map (transformStringPart f) parts)
  ExprExit a kind mStatus -> ExprExit a kind (fmap (transformExpr f) mStatus)
  ExprThrow a e -> ExprThrow a (transformExpr f e)
  ExprConstFetch a qn -> ExprConstFetch a qn

-- | Transform array or list item recursively.
transformArrayItem :: (Expr a -> Expr a) -> ArrayItem a -> ArrayItem a
transformArrayItem f = \case
  item@(ArrayItem {}) -> item { itemKey = fmap (transformExpr f) (itemKey item), itemValue = transformExpr f (itemValue item) }
  item@(ArrayItemEmpty _) -> item

-- | Transform string part in interpolated string.
transformStringPart :: (Expr a -> Expr a) -> StringPart a -> StringPart a
transformStringPart f = \case
  StrLit t -> StrLit t
  StrExpr e -> StrExpr (transformExpr f e)

-- | Transform attribute group recursively.
transformAttributeGroup :: (Expr a -> Expr a) -> AttributeGroup a -> AttributeGroup a
transformAttributeGroup f (AttributeGroup ann attrs) =
  AttributeGroup ann (map (transformAttribute f) attrs)

-- | Transform attribute recursively.
transformAttribute :: (Expr a -> Expr a) -> Attribute a -> Attribute a
transformAttribute f (Attribute ann name args) =
  Attribute ann name (map (transformArg f) args)

-- | Transform argument expression.
transformArg :: (Expr a -> Expr a) -> Arg a -> Arg a
transformArg f arg = arg { argExpr = transformExpr f (argExpr arg) }

-- | Transform parameter recursively.
transformParam :: (Expr a -> Expr a) -> Param a -> Param a
transformParam f p = p
  { paramAttrs = map (transformAttributeGroup f) (paramAttrs p)
  , paramDefault = fmap (transformExpr f) (paramDefault p)
  }

-- | Transform class members recursively.
transformClassMember :: (Expr a -> Expr a) -> ClassMember a -> ClassMember a
transformClassMember f = \case
  MemberProperty p ->
    let attrs' = map (transformAttributeGroup f) (propAttrs p)
        items' = map (\(n, me) -> (n, fmap (transformExpr f) me)) (propItems p)
        hooks' = map (\h -> h
          { hookAttrs = map (transformAttributeGroup f) (hookAttrs h)
          , hookBody = transformHookBody f (hookBody h)
          }) (propHooks p)
    in MemberProperty p { propAttrs = attrs', propItems = items', propHooks = hooks' }
  MemberMethod m ->
    let attrs' = map (transformAttributeGroup f) (methodAttrs m)
        params' = map (transformParam f) (methodParams m)
        body' = fmap (map (transformStmt f)) (methodBody m)
    in MemberMethod m { methodAttrs = attrs', methodParams = params', methodBody = body' }
  MemberConst c ->
    let attrs' = map (transformAttributeGroup f) (constAttrs c)
        items' = map (\(n, e) -> (n, transformExpr f e)) (constItems c)
    in MemberConst c { constAttrs = attrs', constItems = items' }
  MemberTraitUse tu -> MemberTraitUse tu
  MemberEnumCase ec ->
    let attrs' = map (transformAttributeGroup f) (enumCaseAttrs ec)
        val' = fmap (transformExpr f) (enumCaseVal ec)
    in MemberEnumCase ec { enumCaseAttrs = attrs', enumCaseVal = val' }

-- | Transform hook body.
transformHookBody :: (Expr a -> Expr a) -> HookBody a -> HookBody a
transformHookBody f = \case
  HookExpr e -> HookExpr (transformExpr f e)
  HookBlock stmts -> HookBlock (map (transformStmt f) stmts)
  HookAbstract -> HookAbstract

-- | Transform statements recursively.
--
-- Traverses statements and embedded expressions. Static variable declarations
-- (@static $var = $val;@) declare variables in the current function or method
-- scope and have their variable names rewritten via @f@ in lockstep with body
-- occurrences, alongside any initializer expressions. Catch clause variable
-- bindings (@catch (Exception $var)@) have their variable names rewritten via
-- @f@ in lockstep with catch body occurrences.
transformStmt :: (Expr a -> Expr a) -> Stmt a -> Stmt a
transformStmt f = \case
  StmtExpr a e -> StmtExpr a (transformExpr f e)
  StmtBlock a ss -> StmtBlock a (map (transformStmt f) ss)
  StmtIf a c thens elifs mElse ->
    let c' = transformExpr f c
        thens' = map (transformStmt f) thens
        elifs' = map (\(cond, stmts) -> (transformExpr f cond, map (transformStmt f) stmts)) elifs
        mElse' = fmap (map (transformStmt f)) mElse
    in StmtIf a c' thens' elifs' mElse'
  StmtWhile a c ss -> StmtWhile a (transformExpr f c) (map (transformStmt f) ss)
  StmtDoWhile a ss c -> StmtDoWhile a (map (transformStmt f) ss) (transformExpr f c)
  StmtFor a inits conds incrs ss ->
    StmtFor a (map (transformExpr f) inits) (map (transformExpr f) conds) (map (transformExpr f) incrs) (map (transformStmt f) ss)
  StmtForeach a arr mKey val byRef ss ->
    StmtForeach a (transformExpr f arr) (fmap (transformExpr f) mKey) (transformExpr f val) byRef (map (transformStmt f) ss)
  StmtSwitch a c cases ->
    let c' = transformExpr f c
        cases' = map (\case
          SwitchCase ann ce ss -> SwitchCase ann (transformExpr f ce) (map (transformStmt f) ss)
          SwitchDefault ann ss -> SwitchDefault ann (map (transformStmt f) ss)) cases
    in StmtSwitch a c' cases'
  StmtBreak a me -> StmtBreak a (fmap (transformExpr f) me)
  StmtContinue a me -> StmtContinue a (fmap (transformExpr f) me)
  StmtReturn a me -> StmtReturn a (fmap (transformExpr f) me)
  StmtThrowStmt a e -> StmtThrowStmt a (transformExpr f e)
  StmtTry a tryStmts catches mFinally ->
    let tryStmts' = map (transformStmt f) tryStmts
        catches' = map (\c ->
          let var' = fmap (\vn@(VarName va _) ->
                case f (ExprVar va (SimpleVar va vn)) of
                  ExprVar _ (SimpleVar _ newVn) -> newVn
                  _                             -> vn) (catchVar c)
              body' = map (transformStmt f) (catchBody c)
          in c { catchVar = var', catchBody = body' }) catches
        mFinally' = fmap (map (transformStmt f)) mFinally
    in StmtTry a tryStmts' catches' mFinally'
  StmtNamespace a mName mStmts ->
    StmtNamespace a mName (fmap (map (transformStmt f)) mStmts)
  StmtUse a ut ucs -> StmtUse a ut ucs
  StmtGroupUse a ut qn ucs -> StmtGroupUse a ut qn ucs
  StmtConst a c ->
    let attrs' = map (transformAttributeGroup f) (constAttrs c)
        items' = map (\(n, e) -> (n, transformExpr f e)) (constItems c)
    in StmtConst a c { constAttrs = attrs', constItems = items' }
  StmtFunction a fn ->
    let attrs' = map (transformAttributeGroup f) (funcAttrs fn)
        params' = map (transformParam f) (funcParams fn)
        body' = map (transformStmt f) (funcBody fn)
    in StmtFunction a fn { funcAttrs = attrs', funcParams = params', funcBody = body' }
  StmtClass a cd ->
    let attrs' = map (transformAttributeGroup f) (classAttrs cd)
        members' = map (transformClassMember f) (classMembers cd)
    in StmtClass a cd { classAttrs = attrs', classMembers = members' }
  StmtInterface a id' ->
    let attrs' = map (transformAttributeGroup f) (ifaceAttrs id')
        members' = map (transformClassMember f) (ifaceMembers id')
    in StmtInterface a id' { ifaceAttrs = attrs', ifaceMembers = members' }
  StmtTrait a td ->
    let attrs' = map (transformAttributeGroup f) (traitAttrs td)
        members' = map (transformClassMember f) (traitMembers td)
    in StmtTrait a td { traitAttrs = attrs', traitMembers = members' }
  StmtEnum a ed ->
    let attrs' = map (transformAttributeGroup f) (enumAttrs ed)
        members' = map (transformClassMember f) (enumMembers ed)
    in StmtEnum a ed { enumAttrs = attrs', enumMembers = members' }
  StmtEcho a es -> StmtEcho a (map (transformExpr f) es)
  StmtGlobal a es -> StmtGlobal a (map (transformExpr f) es)
  StmtStatic a items ->
    let items' = map (\(vn@(VarName va _), me) ->
          let vn' = case f (ExprVar va (SimpleVar va vn)) of
                ExprVar _ (SimpleVar _ newVn) -> newVn
                _                             -> vn
              me' = fmap (transformExpr f) me
          in (vn', me')) items
    in StmtStatic a items'
  StmtDeclare a dirs mBody ->
    StmtDeclare a (map (\d -> d { declareDirectiveValue = transformExpr f (declareDirectiveValue d) }) dirs)
                  (fmap (map (transformStmt f)) mBody)
  StmtGoto a lbl -> StmtGoto a lbl
  StmtLabel a lbl -> StmtLabel a lbl
  StmtUnset a es -> StmtUnset a (map (transformExpr f) es)
  StmtInlineHtml a t -> StmtInlineHtml a t
  StmtHaltCompiler a t -> StmtHaltCompiler a t
  StmtEmpty a -> StmtEmpty a

-- | Unconditional monoidal query over expressions: applies @q@ to every node,
-- regardless of whether an ancestor already matched. Used internally by
-- 'allExprs' and 'foldExpr', which require an exact per-node visit count.
--
-- This is /not/ the right traversal for a query built by fully recursing
-- over each matched node's subtree (like 'allVariables'): composing such a
-- query into an unconditional traversal double- and triple-counts matches,
-- since every ancestor of a match independently rediscovers it. See
-- 'queryExpr' for the traversal that avoids this.
queryExprUnconditional :: Monoid m => (Expr a -> m) -> Expr a -> m
queryExprUnconditional q = queryExprWith q (queryStmtUnconditional q)

-- | Internal expression traversal with an explicit statement visitor.  The
-- separate visitor lets 'foldStmt' reuse the expression traversal without
-- changing the public query API.
queryExprWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> Expr a -> m
queryExprWith qExpr qStmt expr =
  qExpr expr <> exprChildren (queryExprWith qExpr qStmt) qStmt expr

-- | Unconditional query over expressions embedded in statements. See
-- 'queryExprUnconditional' for why this exists alongside 'queryStmt'.
queryStmtUnconditional :: Monoid m => (Expr a -> m) -> Stmt a -> m
queryStmtUnconditional q =
  stmtChildren (queryExprUnconditional q) (queryStmtUnconditional q)

-- | Accumulate over every expression in preorder, without pruning matching subtrees.
foldExpr :: Monoid m => (Expr a -> m) -> Expr a -> m
foldExpr = queryExprUnconditional

-- | Accumulate over every expression in a statement in traversal order, without
-- pruning matching subtrees. Static declarations, catch bindings and closure
-- use-clause bindings are represented as synthetic variable expressions.
foldStmtExprs :: Monoid m => (Expr a -> m) -> Stmt a -> m
foldStmtExprs = queryStmtUnconditional

queryAttributeGroupWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> AttributeGroup a -> m
queryAttributeGroupWith qExpr qStmt = queryAttributeGroupChildren (queryExprWith qExpr qStmt)

queryParamWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> Param a -> m
queryParamWith qExpr qStmt = queryParamChildren (queryExprWith qExpr qStmt)

-- | Map and accumulate over all statements.
foldStmt :: Monoid m => (Stmt a -> m) -> Stmt a -> m
foldStmt q s = q s <> case s of
  StmtExpr _ e -> foldExprStmts q e
  StmtBlock _ ss -> foldMap (foldStmt q) ss
  StmtIf _ c thens elifs mElse ->
    foldExprStmts q c <>
    foldMap (foldStmt q) thens <>
    foldMap (\(cond, stmts) -> foldExprStmts q cond <> foldMap (foldStmt q) stmts) elifs <>
    maybe mempty (foldMap (foldStmt q)) mElse
  StmtWhile _ c ss -> foldExprStmts q c <> foldMap (foldStmt q) ss
  StmtDoWhile _ ss c -> foldMap (foldStmt q) ss <> foldExprStmts q c
  StmtFor _ inits conds incrs ss ->
    foldMap (foldExprStmts q) inits <>
    foldMap (foldExprStmts q) conds <>
    foldMap (foldExprStmts q) incrs <>
    foldMap (foldStmt q) ss
  StmtForeach _ arr mKey val _ ss ->
    foldExprStmts q arr <>
    maybe mempty (foldExprStmts q) mKey <>
    foldExprStmts q val <>
    foldMap (foldStmt q) ss
  StmtSwitch _ c cases ->
    foldExprStmts q c <> foldMap (\case
      SwitchCase _ ce ss -> foldExprStmts q ce <> foldMap (foldStmt q) ss
      SwitchDefault _ ss -> foldMap (foldStmt q) ss) cases
  StmtTry _ tryStmts catches mFinally ->
    foldMap (foldStmt q) tryStmts <>
    foldMap (foldMap (foldStmt q) . catchBody) catches <>
    maybe mempty (foldMap (foldStmt q)) mFinally
  StmtNamespace _ _ mStmts -> maybe mempty (foldMap (foldStmt q)) mStmts
  StmtBreak _ me -> maybe mempty (foldExprStmts q) me
  StmtContinue _ me -> maybe mempty (foldExprStmts q) me
  StmtReturn _ me -> maybe mempty (foldExprStmts q) me
  StmtThrowStmt _ e -> foldExprStmts q e
  StmtConst _ c ->
    foldMap (queryAttributeGroupWith (foldExprStmts q) (foldStmt q)) (constAttrs c) <>
    foldMap (foldExprStmts q . snd) (constItems c)
  StmtFunction _ fn ->
    foldMap (queryAttributeGroupWith (foldExprStmts q) (foldStmt q)) (funcAttrs fn) <>
    foldMap (queryParamWith (foldExprStmts q) (foldStmt q)) (funcParams fn) <>
    foldMap (foldStmt q) (funcBody fn)
  StmtClass _ cd ->
    foldMap (queryAttributeGroupWith (foldExprStmts q) (foldStmt q)) (classAttrs cd) <>
    foldMap (foldClassMember q) (classMembers cd)
  StmtInterface _ id' ->
    foldMap (queryAttributeGroupWith (foldExprStmts q) (foldStmt q)) (ifaceAttrs id') <>
    foldMap (foldClassMember q) (ifaceMembers id')
  StmtTrait _ td ->
    foldMap (queryAttributeGroupWith (foldExprStmts q) (foldStmt q)) (traitAttrs td) <>
    foldMap (foldClassMember q) (traitMembers td)
  StmtEnum _ ed ->
    foldMap (queryAttributeGroupWith (foldExprStmts q) (foldStmt q)) (enumAttrs ed) <>
    foldMap (foldClassMember q) (enumMembers ed)
  StmtEcho _ es -> foldMap (foldExprStmts q) es
  StmtGlobal _ es -> foldMap (foldExprStmts q) es
  StmtStatic _ items -> foldMap (maybe mempty (foldExprStmts q) . snd) items
  StmtDeclare _ dirs mBody ->
    foldMap (foldExprStmts q . declareDirectiveValue) dirs <>
    maybe mempty (foldMap (foldStmt q)) mBody
  StmtUnset _ es -> foldMap (foldExprStmts q) es
  StmtUse _ _ _ -> mempty
  StmtGroupUse _ _ _ _ -> mempty
  StmtGoto _ _ -> mempty
  StmtLabel _ _ -> mempty
  StmtInlineHtml _ _ -> mempty
  StmtHaltCompiler _ _ -> mempty
  StmtEmpty _ -> mempty

-- | Fold statements reachable through an expression.
foldExprStmts :: Monoid m => (Stmt a -> m) -> Expr a -> m
foldExprStmts q = queryExprWith (const mempty) (foldStmt q)

-- | Fold statements contained in class members.
foldClassMember :: Monoid m => (Stmt a -> m) -> ClassMember a -> m
foldClassMember q = queryClassMemberChildren (queryExprWith (foldExprStmts q) (foldStmt q)) (foldStmt q)


-- | Structural children of an expression, queried via the given recursive
-- callbacks. Exact and pruning queries share this child structure; their
-- recursive callbacks determine whether traversal continues after a match.
exprChildren :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> Expr a -> m
exprChildren recE recS expr = case expr of
  ExprVar _ v -> case v of
    DynamicVar _ e -> recE e
    SimpleVar {} -> mempty
  ExprLit _ l -> case l of
    LitInterpolated _ parts -> foldMap (queryStringPartChildren recE) parts
    LitHeredocInterpolated _ _ parts -> foldMap (queryStringPartChildren recE) parts
    _ -> mempty
  ExprBinary _ _ e1 e2 -> recE e1 <> recE e2
  ExprUnary _ _ e -> recE e
  ExprAssign _ _ e1 e2 -> recE e1 <> recE e2
  ExprAssignRef _ e1 e2 -> recE e1 <> recE e2
  ExprTernary _ cond tExpr fExpr ->
    recE cond <> maybe mempty recE tExpr <> recE fExpr
  ExprNullCoalesce _ e1 e2 -> recE e1 <> recE e2
  ExprClone _ e mWith -> recE e <> maybe mempty recE mWith
  ExprNew _ target args ->
    (case target of ClassTargetExpr e -> recE e; _ -> mempty) <>
    foldMap (queryArgChildren recE) args
  ExprNewAnonClass _ attrs _ args _ _ members ->
    foldMap (queryAttributeGroupChildren recE) attrs <>
    foldMap (queryArgChildren recE) args <>
    foldMap (queryClassMemberChildren recE recS) members
  ExprCall _ fn args ->
    recE fn <> case args of
      ArgsList as -> foldMap (queryArgChildren recE) as
      FirstClassCallable -> mempty
  ExprMethodCall _ obj member args ->
    recE obj <>
    (case member of MemberExpr e -> recE e; _ -> mempty) <>
    (case args of ArgsList as -> foldMap (queryArgChildren recE) as; FirstClassCallable -> mempty)
  ExprNullsafeMethodCall _ obj member args ->
    recE obj <>
    (case member of MemberExpr e -> recE e; _ -> mempty) <>
    (case args of ArgsList as -> foldMap (queryArgChildren recE) as; FirstClassCallable -> mempty)
  ExprPropertyFetch _ obj member ->
    recE obj <> (case member of MemberExpr e -> recE e; _ -> mempty)
  ExprNullsafePropertyFetch _ obj member ->
    recE obj <> (case member of MemberExpr e -> recE e; _ -> mempty)
  ExprStaticCall _ target member args ->
    (case target of ClassTargetExpr e -> recE e; _ -> mempty) <>
    (case member of MemberExpr e -> recE e; _ -> mempty) <>
    (case args of ArgsList as -> foldMap (queryArgChildren recE) as; FirstClassCallable -> mempty)
  ExprStaticPropertyFetch _ target _ ->
    case target of ClassTargetExpr e -> recE e; _ -> mempty
  ExprClassConstFetch _ target constName ->
    (case target of ClassTargetExpr e -> recE e; _ -> mempty) <>
    (case constName of ConstNameDynamic e -> recE e; _ -> mempty)
  ExprArray _ items -> foldMap (queryArrayItemChildren recE) items
  ExprList _ items -> foldMap (queryArrayItemChildren recE) items
  ExprArrayAccess _ arr mIdx -> recE arr <> maybe mempty recE mIdx
  ExprMatch _ subject arms ->
    recE subject <> foldMap (\case
      MatchArm _ conds res -> foldMap recE conds <> recE res
      MatchDefault _ res -> recE res) arms
  ExprClosure _ attrs _ _ params uses _ stmts ->
    foldMap (queryAttributeGroupChildren recE) attrs <>
    foldMap (queryParamChildren recE) params <>
    foldMap (\(vn@(VarName va _), _) -> recE (ExprVar va (SimpleVar va vn))) uses <>
    foldMap recS stmts
  ExprArrowFunction _ attrs _ _ params _ body ->
    foldMap (queryAttributeGroupChildren recE) attrs <>
    foldMap (queryParamChildren recE) params <>
    recE body
  ExprYield _ mK mV -> maybe mempty recE mK <> maybe mempty recE mV
  ExprYieldFrom _ e -> recE e
  ExprCast _ _ e -> recE e
  ExprIsset _ es -> foldMap recE es
  ExprEmpty _ e -> recE e
  ExprEval _ e -> recE e
  ExprInclude _ _ e -> recE e
  ExprPrint _ e -> recE e
  ExprShellExec _ parts -> foldMap (queryStringPartChildren recE) parts
  ExprExit _ _ mStatus -> foldMap recE mStatus
  ExprThrow _ e -> recE e
  ExprConstFetch _ _ -> mempty

queryAttributeGroupChildren :: Monoid m => (Expr a -> m) -> AttributeGroup a -> m
queryAttributeGroupChildren recE (AttributeGroup _ attrs) = foldMap (queryAttributeChildren recE) attrs

queryAttributeChildren :: Monoid m => (Expr a -> m) -> Attribute a -> m
queryAttributeChildren recE (Attribute _ _ args) = foldMap (queryArgChildren recE) args

queryArgChildren :: (Expr a -> m) -> Arg a -> m
queryArgChildren recE = recE . argExpr

queryParamChildren :: Monoid m => (Expr a -> m) -> Param a -> m
queryParamChildren recE p =
  foldMap (queryAttributeGroupChildren recE) (paramAttrs p) <>
  maybe mempty recE (paramDefault p)

queryStringPartChildren :: Monoid m => (Expr a -> m) -> StringPart a -> m
queryStringPartChildren recE = \case
  StrLit _ -> mempty
  StrExpr e -> recE e

queryArrayItemChildren :: Monoid m => (Expr a -> m) -> ArrayItem a -> m
queryArrayItemChildren recE = \case
  ArrayItem _ mKey val _ _ -> maybe mempty recE mKey <> recE val
  ArrayItemEmpty _ -> mempty

queryClassMemberChildren :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> ClassMember a -> m
queryClassMemberChildren recE recS = \case
  MemberProperty p ->
    foldMap (queryAttributeGroupChildren recE) (propAttrs p) <>
    foldMap (maybe mempty recE . snd) (propItems p) <>
    foldMap (\h ->
      foldMap (queryAttributeGroupChildren recE) (hookAttrs h) <>
      case hookBody h of
        HookExpr e -> recE e
        HookBlock ss -> foldMap recS ss
        HookAbstract -> mempty) (propHooks p)
  MemberMethod m ->
    foldMap (queryAttributeGroupChildren recE) (methodAttrs m) <>
    foldMap (queryParamChildren recE) (methodParams m) <>
    maybe mempty (foldMap recS) (methodBody m)
  MemberConst c ->
    foldMap (queryAttributeGroupChildren recE) (constAttrs c) <>
    foldMap (recE . snd) (constItems c)
  MemberTraitUse _ -> mempty
  MemberEnumCase ec ->
    foldMap (queryAttributeGroupChildren recE) (enumCaseAttrs ec) <>
    maybe mempty recE (enumCaseVal ec)

-- | Structural children of a statement, queried via the given recursive
-- callbacks. See 'exprChildren'.
stmtChildren :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> Stmt a -> m
stmtChildren recE recS = \case
  StmtExpr _ e -> recE e
  StmtBlock _ ss -> foldMap recS ss
  StmtIf _ c thens elifs mElse ->
    recE c <> foldMap recS thens <>
    foldMap (\(cond, stmts) -> recE cond <> foldMap recS stmts) elifs <>
    maybe mempty (foldMap recS) mElse
  StmtWhile _ c ss -> recE c <> foldMap recS ss
  StmtDoWhile _ ss c -> foldMap recS ss <> recE c
  StmtFor _ inits conds incrs ss ->
    foldMap recE inits <> foldMap recE conds <> foldMap recE incrs <> foldMap recS ss
  StmtForeach _ arr mKey val _ ss ->
    recE arr <> maybe mempty recE mKey <> recE val <> foldMap recS ss
  StmtSwitch _ c cases ->
    recE c <> foldMap (\case
      SwitchCase _ ce ss -> recE ce <> foldMap recS ss
      SwitchDefault _ ss -> foldMap recS ss) cases
  StmtBreak _ me -> maybe mempty recE me
  StmtContinue _ me -> maybe mempty recE me
  StmtReturn _ me -> maybe mempty recE me
  StmtThrowStmt _ e -> recE e
  StmtTry _ tryStmts catches mFinally ->
    foldMap recS tryStmts <>
    foldMap (\c ->
      maybe mempty (\(vn@(VarName va _)) -> recE (ExprVar va (SimpleVar va vn))) (catchVar c) <>
      foldMap recS (catchBody c)) catches <>
    maybe mempty (foldMap recS) mFinally
  StmtNamespace _ _ mStmts -> maybe mempty (foldMap recS) mStmts
  StmtUse _ _ _ -> mempty
  StmtGroupUse _ _ _ _ -> mempty
  StmtConst _ c ->
    foldMap (queryAttributeGroupChildren recE) (constAttrs c) <>
    foldMap (recE . snd) (constItems c)
  StmtFunction _ fn ->
    foldMap (queryAttributeGroupChildren recE) (funcAttrs fn) <>
    foldMap (queryParamChildren recE) (funcParams fn) <>
    foldMap recS (funcBody fn)
  StmtClass _ cd ->
    foldMap (queryAttributeGroupChildren recE) (classAttrs cd) <>
    foldMap (queryClassMemberChildren recE recS) (classMembers cd)
  StmtInterface _ id' ->
    foldMap (queryAttributeGroupChildren recE) (ifaceAttrs id') <>
    foldMap (queryClassMemberChildren recE recS) (ifaceMembers id')
  StmtTrait _ td ->
    foldMap (queryAttributeGroupChildren recE) (traitAttrs td) <>
    foldMap (queryClassMemberChildren recE recS) (traitMembers td)
  StmtEnum _ ed ->
    foldMap (queryAttributeGroupChildren recE) (enumAttrs ed) <>
    foldMap (queryClassMemberChildren recE recS) (enumMembers ed)
  StmtEcho _ es -> foldMap recE es
  StmtGlobal _ es -> foldMap recE es
  StmtStatic _ items ->
    foldMap (\(vn@(VarName va _), me) -> recE (ExprVar va (SimpleVar va vn)) <> maybe mempty recE me) items
  StmtDeclare _ dirs mBody ->
    foldMap (recE . declareDirectiveValue) dirs <> maybe mempty (foldMap recS) mBody
  StmtGoto _ _ -> mempty
  StmtLabel _ _ -> mempty
  StmtUnset _ es -> foldMap recE es
  StmtInlineHtml _ _ -> mempty
  StmtHaltCompiler _ _ -> mempty
  StmtEmpty _ -> mempty

-- | Monoidal query over expressions.
--
-- Closure use-clause bindings (@use ($var, &$ref)@) are traversed as variable
-- references to enclosing scope variables, allowing queries such as 'allVariables'
-- to surface both closure capture bindings and body occurrences.
--
-- Subexpressions embedded within interpolated strings, heredocs and backtick
-- commands ('LitInterpolated', 'LitHeredocInterpolated', 'ExprShellExec') are
-- recursively queried.
--
-- Unlike a naive structural traversal, this query short-circuits: once @q@
-- matches a non-'mempty' result at a node, its subtree is not visited again.
-- This matters when @q@ is itself already a fully recursive query over a
-- subtree (as 'allVariables' is) -- without short-circuiting, every ancestor
-- of a match would independently rediscover it via the framework's own
-- structural recursion, double- and triple-counting matches (issue #215).
-- A query that only matches at leaves it doesn't recurse into (e.g. a plain
-- @\\case ExprVar _ ... -> ...; _ -> mempty@) is unaffected, since it's
-- 'mempty' everywhere except at those leaves.
queryExpr :: (Eq m, Monoid m) => (Expr a -> m) -> Expr a -> m
queryExpr q = queryExprShortCircuit q (queryStmt q)

queryExprShortCircuit :: (Eq m, Monoid m) => (Expr a -> m) -> (Stmt a -> m) -> Expr a -> m
queryExprShortCircuit qExpr qStmt expr =
  let here = qExpr expr
  in if here /= mempty
       then here
       else here <> exprChildren (queryExprShortCircuit qExpr qStmt) qStmt expr

-- | Query expressions in statements, pruning each expression subtree at its
-- first non-'mempty' match. See 'foldStmtExprs' for an exact traversal.
--
-- Static variable declarations (@static $var = $val;@) are queried for variable
-- references, surfacing declared static variable names as well as initializer
-- expressions. Catch clause variable bindings (@catch (Exception $var)@) are
-- queried for variable references, surfacing bound exception variable names
-- alongside catch body expressions.
queryStmt :: (Eq m, Monoid m) => (Expr a -> m) -> Stmt a -> m
queryStmt q = stmtChildren (queryExprShortCircuit q (queryStmt q)) (queryStmt q)

-- | Collect all expressions within an expression.
allExprs :: Expr a -> [Expr a]
allExprs expr = appEndo (queryExprUnconditional (Endo . (:)) expr) []

-- | Collect all variable names within an expression.
allVariables :: Expr a -> [Text]
allVariables expr = appEndo (visitExpr expr) []
  where
    visitExpr e = case e of
      ExprVar _ (SimpleVar _ (VarName _ n)) -> Endo (n :)
      _ -> exprChildren visitExpr visitStmt e
    visitStmt = stmtChildren visitExpr visitStmt
