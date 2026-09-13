{-# LANGUAGE RankNTypes #-}

module Language.PHP.Fold
  ( -- * Annotation stripping and mapping
    stripAnnotations
  , mapAnnotation
    -- * Folds and Catamorphisms
  , foldExpr
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
import Data.Monoid ()

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
-- Subexpressions embedded within interpolated strings ('LitInterpolated') are
-- recursively rewritten.
transformExpr :: (Expr a -> Expr a) -> Expr a -> Expr a
transformExpr f = f . \case
  ExprVar a v -> case v of
    DynamicVar va e -> ExprVar a (DynamicVar va (transformExpr f e))
    SimpleVar {} -> ExprVar a v
  ExprLit a l -> case l of
    LitInterpolated la parts ->
      ExprLit a (LitInterpolated la (map (transformStringPart f) parts))
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
        catches' = map (\c -> c { catchBody = map (transformStmt f) (catchBody c) }) catches
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
    let items' = map (\(v, me) -> (v, fmap (transformExpr f) me)) items
    in StmtStatic a items'
  StmtDeclare a dirs mBody ->
    StmtDeclare a dirs (fmap (map (transformStmt f)) mBody)
  StmtGoto a lbl -> StmtGoto a lbl
  StmtLabel a lbl -> StmtLabel a lbl
  StmtUnset a es -> StmtUnset a (map (transformExpr f) es)
  StmtInlineHtml a t -> StmtInlineHtml a t
  StmtHaltCompiler a t -> StmtHaltCompiler a t
  StmtEmpty a -> StmtEmpty a

-- | Query attribute group.
queryAttributeGroup :: Monoid m => (Expr a -> m) -> AttributeGroup a -> m
queryAttributeGroup q = queryAttributeGroupWith q (queryStmt q)

queryAttributeGroupWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> AttributeGroup a -> m
queryAttributeGroupWith qExpr qStmt (AttributeGroup _ attrs) =
  foldMap (queryAttributeWith qExpr qStmt) attrs

queryAttributeWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> Attribute a -> m
queryAttributeWith qExpr qStmt (Attribute _ _ args) =
  foldMap (queryArgWith qExpr qStmt) args

queryArgWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> Arg a -> m
queryArgWith qExpr qStmt = queryExprWith qExpr qStmt . argExpr

-- | Query parameter.
queryParam :: Monoid m => (Expr a -> m) -> Param a -> m
queryParam q = queryParamWith q (queryStmt q)

queryParamWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> Param a -> m
queryParamWith qExpr qStmt p =
  foldMap (queryAttributeGroupWith qExpr qStmt) (paramAttrs p) <>
  maybe mempty (queryExprWith qExpr qStmt) (paramDefault p)

queryStringPartWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> StringPart a -> m
queryStringPartWith qExpr qStmt = \case
  StrLit _ -> mempty
  StrExpr e -> queryExprWith qExpr qStmt e

queryArrayItemWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> ArrayItem a -> m
queryArrayItemWith qExpr qStmt = \case
  ArrayItem _ mKey val _ _ ->
    maybe mempty (queryExprWith qExpr qStmt) mKey <> queryExprWith qExpr qStmt val
  ArrayItemEmpty _ -> mempty

-- | Monoidal query over expressions.
--
-- Closure use-clause bindings (@use ($var, &$ref)@) are traversed as variable
-- references to enclosing scope variables, allowing queries such as 'allVariables'
-- to surface both closure capture bindings and body occurrences.
--
-- Subexpressions embedded within interpolated strings ('LitInterpolated') are
-- recursively queried.
queryExpr :: Monoid m => (Expr a -> m) -> Expr a -> m
queryExpr q = queryExprWith q (queryStmt q)

-- | Internal expression traversal with an explicit statement visitor.  The
-- separate visitor lets 'foldStmt' reuse the expression traversal without
-- changing the public query API.
queryExprWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> Expr a -> m
queryExprWith qExpr qStmt expr = qExpr expr <> case expr of
  ExprVar _ v -> case v of
    DynamicVar _ e -> queryExprWith qExpr qStmt e
    SimpleVar {} -> mempty
  ExprLit _ l -> case l of
    LitInterpolated _ parts -> foldMap (queryStringPartWith qExpr qStmt) parts
    _ -> mempty
  ExprBinary _ _ e1 e2 -> queryExprWith qExpr qStmt e1 <> queryExprWith qExpr qStmt e2
  ExprUnary _ _ e -> queryExprWith qExpr qStmt e
  ExprAssign _ _ e1 e2 -> queryExprWith qExpr qStmt e1 <> queryExprWith qExpr qStmt e2
  ExprAssignRef _ e1 e2 -> queryExprWith qExpr qStmt e1 <> queryExprWith qExpr qStmt e2
  ExprTernary _ cond tExpr fExpr ->
    queryExprWith qExpr qStmt cond <>
    maybe mempty (queryExprWith qExpr qStmt) tExpr <>
    queryExprWith qExpr qStmt fExpr
  ExprNullCoalesce _ e1 e2 -> queryExprWith qExpr qStmt e1 <> queryExprWith qExpr qStmt e2
  ExprClone _ e mWith ->
    queryExprWith qExpr qStmt e <> maybe mempty (queryExprWith qExpr qStmt) mWith
  ExprNew _ target args ->
    (case target of ClassTargetExpr e -> queryExprWith qExpr qStmt e; _ -> mempty) <>
    foldMap (queryArgWith qExpr qStmt) args
  ExprNewAnonClass _ attrs _ args _ _ members ->
    foldMap (queryAttributeGroupWith qExpr qStmt) attrs <>
    foldMap (queryArgWith qExpr qStmt) args <>
    foldMap (queryClassMemberWith qExpr qStmt) members
  ExprCall _ fn args ->
    queryExprWith qExpr qStmt fn <> case args of
      ArgsList as -> foldMap (queryArgWith qExpr qStmt) as
      FirstClassCallable -> mempty
  ExprMethodCall _ obj member args ->
    queryExprWith qExpr qStmt obj <>
    (case member of MemberExpr e -> queryExprWith qExpr qStmt e; _ -> mempty) <>
    (case args of ArgsList as -> foldMap (queryArgWith qExpr qStmt) as; FirstClassCallable -> mempty)
  ExprNullsafeMethodCall _ obj member args ->
    queryExprWith qExpr qStmt obj <>
    (case member of MemberExpr e -> queryExprWith qExpr qStmt e; _ -> mempty) <>
    (case args of ArgsList as -> foldMap (queryArgWith qExpr qStmt) as; FirstClassCallable -> mempty)
  ExprPropertyFetch _ obj member ->
    queryExprWith qExpr qStmt obj <>
    (case member of MemberExpr e -> queryExprWith qExpr qStmt e; _ -> mempty)
  ExprNullsafePropertyFetch _ obj member ->
    queryExprWith qExpr qStmt obj <>
    (case member of MemberExpr e -> queryExprWith qExpr qStmt e; _ -> mempty)
  ExprStaticCall _ target member args ->
    (case target of ClassTargetExpr e -> queryExprWith qExpr qStmt e; _ -> mempty) <>
    (case member of MemberExpr e -> queryExprWith qExpr qStmt e; _ -> mempty) <>
    (case args of ArgsList as -> foldMap (queryArgWith qExpr qStmt) as; FirstClassCallable -> mempty)
  ExprStaticPropertyFetch _ target _ ->
    case target of ClassTargetExpr e -> queryExprWith qExpr qStmt e; _ -> mempty
  ExprClassConstFetch _ target constName ->
    (case target of ClassTargetExpr e -> queryExprWith qExpr qStmt e; _ -> mempty) <>
    (case constName of ConstNameDynamic e -> queryExprWith qExpr qStmt e; _ -> mempty)
  ExprArray _ items ->
    foldMap (queryArrayItemWith qExpr qStmt) items
  ExprList _ items ->
    foldMap (queryArrayItemWith qExpr qStmt) items
  ExprArrayAccess _ arr mIdx ->
    queryExprWith qExpr qStmt arr <> maybe mempty (queryExprWith qExpr qStmt) mIdx
  ExprMatch _ subject arms ->
    queryExprWith qExpr qStmt subject <> foldMap (\case
      MatchArm _ conds res -> foldMap (queryExprWith qExpr qStmt) conds <> queryExprWith qExpr qStmt res
      MatchDefault _ res -> queryExprWith qExpr qStmt res) arms
  ExprClosure _ attrs _ _ params uses _ stmts ->
    foldMap (queryAttributeGroupWith qExpr qStmt) attrs <>
    foldMap (queryParamWith qExpr qStmt) params <>
    foldMap (\(vn@(VarName va _), _) -> queryExprWith qExpr qStmt (ExprVar va (SimpleVar va vn))) uses <>
    foldMap qStmt stmts
  ExprArrowFunction _ attrs _ _ params _ body ->
    foldMap (queryAttributeGroupWith qExpr qStmt) attrs <>
    foldMap (queryParamWith qExpr qStmt) params <>
    queryExprWith qExpr qStmt body
  ExprYield _ mK mV ->
    maybe mempty (queryExprWith qExpr qStmt) mK <> maybe mempty (queryExprWith qExpr qStmt) mV
  ExprYieldFrom _ e -> queryExprWith qExpr qStmt e
  ExprCast _ _ e -> queryExprWith qExpr qStmt e
  ExprIsset _ es -> foldMap (queryExprWith qExpr qStmt) es
  ExprEmpty _ e -> queryExprWith qExpr qStmt e
  ExprEval _ e -> queryExprWith qExpr qStmt e
  ExprInclude _ _ e -> queryExprWith qExpr qStmt e
  ExprPrint _ e -> queryExprWith qExpr qStmt e
  ExprExit _ _ mStatus -> foldMap (queryExprWith qExpr qStmt) mStatus
  ExprThrow _ e -> queryExprWith qExpr qStmt e
  ExprConstFetch _ _ -> mempty

-- | Query expressions in statements.
queryStmt :: Monoid m => (Expr a -> m) -> Stmt a -> m
queryStmt q = \case
  StmtExpr _ e -> queryExpr q e
  StmtBlock _ ss -> foldMap (queryStmt q) ss
  StmtIf _ c thens elifs mElse ->
    queryExpr q c <> foldMap (queryStmt q) thens <>
    foldMap (\(cond, stmts) -> queryExpr q cond <> foldMap (queryStmt q) stmts) elifs <>
    maybe mempty (foldMap (queryStmt q)) mElse
  StmtWhile _ c ss -> queryExpr q c <> foldMap (queryStmt q) ss
  StmtDoWhile _ ss c -> foldMap (queryStmt q) ss <> queryExpr q c
  StmtFor _ inits conds incrs ss ->
    foldMap (queryExpr q) inits <> foldMap (queryExpr q) conds <>
    foldMap (queryExpr q) incrs <> foldMap (queryStmt q) ss
  StmtForeach _ arr mKey val _ ss ->
    queryExpr q arr <> maybe mempty (queryExpr q) mKey <> queryExpr q val <> foldMap (queryStmt q) ss
  StmtSwitch _ c cases ->
    queryExpr q c <> foldMap (\case
      SwitchCase _ ce ss -> queryExpr q ce <> foldMap (queryStmt q) ss
      SwitchDefault _ ss -> foldMap (queryStmt q) ss) cases
  StmtBreak _ me -> maybe mempty (queryExpr q) me
  StmtContinue _ me -> maybe mempty (queryExpr q) me
  StmtReturn _ me -> maybe mempty (queryExpr q) me
  StmtThrowStmt _ e -> queryExpr q e
  StmtTry _ tryStmts catches mFinally ->
    foldMap (queryStmt q) tryStmts <>
    foldMap (\c -> foldMap (queryStmt q) (catchBody c)) catches <>
    maybe mempty (foldMap (queryStmt q)) mFinally
  StmtNamespace _ _ mStmts -> maybe mempty (foldMap (queryStmt q)) mStmts
  StmtUse _ _ _ -> mempty
  StmtGroupUse _ _ _ _ -> mempty
  StmtConst _ c ->
    foldMap (queryAttributeGroup q) (constAttrs c) <>
    foldMap (queryExpr q . snd) (constItems c)
  StmtFunction _ fn ->
    foldMap (queryAttributeGroup q) (funcAttrs fn) <>
    foldMap (queryParam q) (funcParams fn) <>
    foldMap (queryStmt q) (funcBody fn)
  StmtClass _ cd ->
    foldMap (queryAttributeGroup q) (classAttrs cd) <>
    foldMap (queryClassMember q) (classMembers cd)
  StmtInterface _ id' ->
    foldMap (queryAttributeGroup q) (ifaceAttrs id') <>
    foldMap (queryClassMember q) (ifaceMembers id')
  StmtTrait _ td ->
    foldMap (queryAttributeGroup q) (traitAttrs td) <>
    foldMap (queryClassMember q) (traitMembers td)
  StmtEnum _ ed ->
    foldMap (queryAttributeGroup q) (enumAttrs ed) <>
    foldMap (queryClassMember q) (enumMembers ed)
  StmtEcho _ es -> foldMap (queryExpr q) es
  StmtGlobal _ es -> foldMap (queryExpr q) es
  StmtStatic _ items -> foldMap (maybe mempty (queryExpr q) . snd) items
  StmtDeclare _ _ mBody -> maybe mempty (foldMap (queryStmt q)) mBody
  StmtGoto _ _ -> mempty
  StmtLabel _ _ -> mempty
  StmtUnset _ es -> foldMap (queryExpr q) es
  StmtInlineHtml _ _ -> mempty
  StmtHaltCompiler _ _ -> mempty
  StmtEmpty _ -> mempty

-- | Query expressions in class members.
queryClassMember :: Monoid m => (Expr a -> m) -> ClassMember a -> m
queryClassMember q = queryClassMemberWith q (queryStmt q)

queryClassMemberWith :: Monoid m => (Expr a -> m) -> (Stmt a -> m) -> ClassMember a -> m
queryClassMemberWith qExpr qStmt = \case
  MemberProperty p ->
    foldMap (queryAttributeGroupWith qExpr qStmt) (propAttrs p) <>
    foldMap (maybe mempty (queryExprWith qExpr qStmt) . snd) (propItems p) <>
    foldMap (\h ->
      foldMap (queryAttributeGroupWith qExpr qStmt) (hookAttrs h) <>
      case hookBody h of
        HookExpr e -> queryExprWith qExpr qStmt e
        HookBlock ss -> foldMap qStmt ss
        HookAbstract -> mempty) (propHooks p)
  MemberMethod m ->
    foldMap (queryAttributeGroupWith qExpr qStmt) (methodAttrs m) <>
    foldMap (queryParamWith qExpr qStmt) (methodParams m) <>
    maybe mempty (foldMap qStmt) (methodBody m)
  MemberConst c ->
    foldMap (queryAttributeGroupWith qExpr qStmt) (constAttrs c) <>
    foldMap (queryExprWith qExpr qStmt . snd) (constItems c)
  MemberTraitUse _ -> mempty
  MemberEnumCase ec ->
    foldMap (queryAttributeGroupWith qExpr qStmt) (enumCaseAttrs ec) <>
    maybe mempty (queryExprWith qExpr qStmt) (enumCaseVal ec)

-- | Catamorphism over expressions using an expression transformation or reduction.
foldExpr :: Monoid m => (Expr a -> m) -> Expr a -> m
foldExpr = queryExpr

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
  StmtDeclare _ _ mBody -> maybe mempty (foldMap (foldStmt q)) mBody
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
foldClassMember q = queryClassMemberWith (foldExprStmts q) (foldStmt q)


-- | Collect all expressions within an expression.
allExprs :: Expr a -> [Expr a]
allExprs = queryExpr (: [])

-- | Collect all variable names within an expression.
allVariables :: Expr a -> [Text]
allVariables = queryExpr (\case
  ExprVar _ (SimpleVar _ (VarName _ n)) -> [n]
  _ -> [])
