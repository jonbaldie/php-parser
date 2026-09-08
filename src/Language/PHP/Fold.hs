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
transformExpr :: (Expr a -> Expr a) -> Expr a -> Expr a
transformExpr f = f . \case
  ExprVar a v -> case v of
    DynamicVar va e -> ExprVar a (DynamicVar va (transformExpr f e))
    SimpleVar {} -> ExprVar a v
  ExprLit a l -> ExprLit a l
  ExprBinary a op e1 e2 -> ExprBinary a op (transformExpr f e1) (transformExpr f e2)
  ExprUnary a op e -> ExprUnary a op (transformExpr f e)
  ExprAssign a mOp e1 e2 -> ExprAssign a mOp (transformExpr f e1) (transformExpr f e2)
  ExprTernary a cond tExpr fExpr ->
    ExprTernary a (transformExpr f cond) (fmap (transformExpr f) tExpr) (transformExpr f fExpr)
  ExprNullCoalesce a e1 e2 -> ExprNullCoalesce a (transformExpr f e1) (transformExpr f e2)
  ExprClone a e mWith ->
    ExprClone a (transformExpr f e) (fmap (map (\(k, v) -> (transformExpr f k, transformExpr f v))) mWith)
  ExprNew a target args ->
    let target' = case target of
          ClassTargetExpr e -> ClassTargetExpr (transformExpr f e)
          t -> t
        args' = map (\arg -> arg { argExpr = transformExpr f (argExpr arg) }) args
    in ExprNew a target' args'
  ExprNewAnonClass a attrs modif args ext impl members ->
    let args' = map (\arg -> arg { argExpr = transformExpr f (argExpr arg) }) args
        members' = map (transformClassMember f) members
    in ExprNewAnonClass a attrs modif args' ext impl members'
  ExprCall a fn args ->
    let fn' = transformExpr f fn
        args' = case args of
          ArgsList as -> ArgsList (map (\arg -> arg { argExpr = transformExpr f (argExpr arg) }) as)
          FirstClassCallable -> FirstClassCallable
    in ExprCall a fn' args'
  ExprMethodCall a obj member args ->
    let obj' = transformExpr f obj
        member' = case member of
          MemberExpr e -> MemberExpr (transformExpr f e)
          m -> m
        args' = case args of
          ArgsList as -> ArgsList (map (\arg -> arg { argExpr = transformExpr f (argExpr arg) }) as)
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
          ArgsList as -> ArgsList (map (\arg -> arg { argExpr = transformExpr f (argExpr arg) }) as)
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
    let items' = map (\item -> item { itemKey = fmap (transformExpr f) (itemKey item), itemValue = transformExpr f (itemValue item) }) items
    in ExprArray a items'
  ExprArrayAccess a arr mIdx ->
    ExprArrayAccess a (transformExpr f arr) (fmap (transformExpr f) mIdx)
  ExprMatch a subject arms ->
    let subject' = transformExpr f subject
        arms' = map (\case
          MatchArm ann conds res -> MatchArm ann (map (transformExpr f) conds) (transformExpr f res)
          MatchDefault ann res -> MatchDefault ann (transformExpr f res)) arms
    in ExprMatch a subject' arms'
  ExprClosure a attrs byRef isStatic params uses retType stmts ->
    let params' = map (\p -> p { paramDefault = fmap (transformExpr f) (paramDefault p) }) params
        stmts' = map (transformStmt f) stmts
    in ExprClosure a attrs byRef isStatic params' uses retType stmts'
  ExprArrowFunction a attrs byRef isStatic params retType expr ->
    let params' = map (\p -> p { paramDefault = fmap (transformExpr f) (paramDefault p) }) params
    in ExprArrowFunction a attrs byRef isStatic params' retType (transformExpr f expr)
  ExprYield a mK mV ->
    ExprYield a (fmap (transformExpr f) mK) (fmap (transformExpr f) mV)
  ExprYieldFrom a e -> ExprYieldFrom a (transformExpr f e)
  ExprCast a cast e -> ExprCast a cast (transformExpr f e)
  ExprIsset a es -> ExprIsset a (map (transformExpr f) es)
  ExprEmpty a e -> ExprEmpty a (transformExpr f e)
  ExprEval a e -> ExprEval a (transformExpr f e)
  ExprInclude a inc e -> ExprInclude a inc (transformExpr f e)
  ExprThrow a e -> ExprThrow a (transformExpr f e)
  ExprConstFetch a qn -> ExprConstFetch a qn

-- | Transform class members recursively.
transformClassMember :: (Expr a -> Expr a) -> ClassMember a -> ClassMember a
transformClassMember f = \case
  MemberProperty p ->
    let items' = map (\(n, me) -> (n, fmap (transformExpr f) me)) (propItems p)
        hooks' = map (\h -> h { hookBody = transformHookBody f (hookBody h) }) (propHooks p)
    in MemberProperty p { propItems = items', propHooks = hooks' }
  MemberMethod m ->
    let params' = map (\p -> p { paramDefault = fmap (transformExpr f) (paramDefault p) }) (methodParams m)
        body' = fmap (map (transformStmt f)) (methodBody m)
    in MemberMethod m { methodParams = params', methodBody = body' }
  MemberConst c ->
    let items' = map (\(n, e) -> (n, transformExpr f e)) (constItems c)
    in MemberConst c { constItems = items' }
  MemberTraitUse tu -> MemberTraitUse tu
  MemberEnumCase ec ->
    let val' = fmap (transformExpr f) (enumCaseVal ec)
    in MemberEnumCase ec { enumCaseVal = val' }

-- | Transform hook body.
transformHookBody :: (Expr a -> Expr a) -> HookBody a -> HookBody a
transformHookBody f = \case
  HookExpr e -> HookExpr (transformExpr f e)
  HookBlock stmts -> HookBlock (map (transformStmt f) stmts)

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
    let items' = map (\(n, e) -> (n, transformExpr f e)) (constItems c)
    in StmtConst a c { constItems = items' }
  StmtFunction a fn ->
    let params' = map (\p -> p { paramDefault = fmap (transformExpr f) (paramDefault p) }) (funcParams fn)
        body' = map (transformStmt f) (funcBody fn)
    in StmtFunction a fn { funcParams = params', funcBody = body' }
  StmtClass a cd ->
    StmtClass a cd { classMembers = map (transformClassMember f) (classMembers cd) }
  StmtInterface a id' ->
    StmtInterface a id' { ifaceMembers = map (transformClassMember f) (ifaceMembers id') }
  StmtTrait a td ->
    StmtTrait a td { traitMembers = map (transformClassMember f) (traitMembers td) }
  StmtEnum a ed ->
    StmtEnum a ed { enumMembers = map (transformClassMember f) (enumMembers ed) }
  StmtEcho a es -> StmtEcho a (map (transformExpr f) es)
  StmtGlobal a es -> StmtGlobal a (map (transformExpr f) es)
  StmtStatic a items ->
    let items' = map (\(v, me) -> (v, fmap (transformExpr f) me)) items
    in StmtStatic a items'
  StmtInlineHtml a t -> StmtInlineHtml a t
  StmtHaltCompiler a t -> StmtHaltCompiler a t
  StmtEmpty a -> StmtEmpty a

-- | Monoidal query over expressions.
queryExpr :: Monoid m => (Expr a -> m) -> Expr a -> m
queryExpr q expr = q expr <> case expr of
  ExprVar _ v -> case v of
    DynamicVar _ e -> queryExpr q e
    SimpleVar {} -> mempty
  ExprLit _ _ -> mempty
  ExprBinary _ _ e1 e2 -> queryExpr q e1 <> queryExpr q e2
  ExprUnary _ _ e -> queryExpr q e
  ExprAssign _ _ e1 e2 -> queryExpr q e1 <> queryExpr q e2
  ExprTernary _ cond tExpr fExpr ->
    queryExpr q cond <> maybe mempty (queryExpr q) tExpr <> queryExpr q fExpr
  ExprNullCoalesce _ e1 e2 -> queryExpr q e1 <> queryExpr q e2
  ExprClone _ e mWith ->
    queryExpr q e <> maybe mempty (foldMap (\(k, v) -> queryExpr q k <> queryExpr q v)) mWith
  ExprNew _ target args ->
    (case target of ClassTargetExpr e -> queryExpr q e; _ -> mempty) <>
    foldMap (queryExpr q . argExpr) args
  ExprNewAnonClass _ _ _ args _ _ members ->
    foldMap (queryExpr q . argExpr) args <> foldMap (queryClassMember q) members
  ExprCall _ fn args ->
    queryExpr q fn <> case args of
      ArgsList as -> foldMap (queryExpr q . argExpr) as
      FirstClassCallable -> mempty
  ExprMethodCall _ obj member args ->
    queryExpr q obj <> (case member of MemberExpr e -> queryExpr q e; _ -> mempty) <>
    (case args of ArgsList as -> foldMap (queryExpr q . argExpr) as; FirstClassCallable -> mempty)
  ExprNullsafeMethodCall _ obj member args ->
    queryExpr q obj <> (case member of MemberExpr e -> queryExpr q e; _ -> mempty) <>
    (case args of ArgsList as -> foldMap (queryExpr q . argExpr) as; FirstClassCallable -> mempty)
  ExprPropertyFetch _ obj member ->
    queryExpr q obj <> (case member of MemberExpr e -> queryExpr q e; _ -> mempty)
  ExprNullsafePropertyFetch _ obj member ->
    queryExpr q obj <> (case member of MemberExpr e -> queryExpr q e; _ -> mempty)
  ExprStaticCall _ target member args ->
    (case target of ClassTargetExpr e -> queryExpr q e; _ -> mempty) <>
    (case member of MemberExpr e -> queryExpr q e; _ -> mempty) <>
    (case args of ArgsList as -> foldMap (queryExpr q . argExpr) as; FirstClassCallable -> mempty)
  ExprStaticPropertyFetch _ target _ ->
    case target of ClassTargetExpr e -> queryExpr q e; _ -> mempty
  ExprClassConstFetch _ target constName ->
    (case target of ClassTargetExpr e -> queryExpr q e; _ -> mempty) <>
    (case constName of ConstNameDynamic e -> queryExpr q e; _ -> mempty)
  ExprArray _ items ->
    foldMap (\item -> maybe mempty (queryExpr q) (itemKey item) <> queryExpr q (itemValue item)) items
  ExprArrayAccess _ arr mIdx ->
    queryExpr q arr <> maybe mempty (queryExpr q) mIdx
  ExprMatch _ subject arms ->
    queryExpr q subject <> foldMap (\case
      MatchArm _ conds res -> foldMap (queryExpr q) conds <> queryExpr q res
      MatchDefault _ res -> queryExpr q res) arms
  ExprClosure _ _ _ _ params _ _ stmts ->
    foldMap (maybe mempty (queryExpr q) . paramDefault) params <> foldMap (queryStmt q) stmts
  ExprArrowFunction _ _ _ _ params _ body ->
    foldMap (maybe mempty (queryExpr q) . paramDefault) params <> queryExpr q body
  ExprYield _ mK mV ->
    maybe mempty (queryExpr q) mK <> maybe mempty (queryExpr q) mV
  ExprYieldFrom _ e -> queryExpr q e
  ExprCast _ _ e -> queryExpr q e
  ExprIsset _ es -> foldMap (queryExpr q) es
  ExprEmpty _ e -> queryExpr q e
  ExprEval _ e -> queryExpr q e
  ExprInclude _ _ e -> queryExpr q e
  ExprThrow _ e -> queryExpr q e
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
  StmtConst _ c -> foldMap (queryExpr q . snd) (constItems c)
  StmtFunction _ fn ->
    foldMap (maybe mempty (queryExpr q) . paramDefault) (funcParams fn) <>
    foldMap (queryStmt q) (funcBody fn)
  StmtClass _ cd -> foldMap (queryClassMember q) (classMembers cd)
  StmtInterface _ id' -> foldMap (queryClassMember q) (ifaceMembers id')
  StmtTrait _ td -> foldMap (queryClassMember q) (traitMembers td)
  StmtEnum _ ed -> foldMap (queryClassMember q) (enumMembers ed)
  StmtEcho _ es -> foldMap (queryExpr q) es
  StmtGlobal _ es -> foldMap (queryExpr q) es
  StmtStatic _ items -> foldMap (maybe mempty (queryExpr q) . snd) items
  StmtInlineHtml _ _ -> mempty
  StmtHaltCompiler _ _ -> mempty
  StmtEmpty _ -> mempty

-- | Query expressions in class members.
queryClassMember :: Monoid m => (Expr a -> m) -> ClassMember a -> m
queryClassMember q = \case
  MemberProperty p ->
    foldMap (maybe mempty (queryExpr q) . snd) (propItems p) <>
    foldMap (\h -> case hookBody h of
      HookExpr e -> queryExpr q e
      HookBlock ss -> foldMap (queryStmt q) ss) (propHooks p)
  MemberMethod m ->
    foldMap (maybe mempty (queryExpr q) . paramDefault) (methodParams m) <>
    maybe mempty (foldMap (queryStmt q)) (methodBody m)
  MemberConst c -> foldMap (queryExpr q . snd) (constItems c)
  MemberTraitUse _ -> mempty
  MemberEnumCase ec -> maybe mempty (queryExpr q) (enumCaseVal ec)

-- | Catamorphism over expressions using an expression transformation or reduction.
foldExpr :: Monoid m => (Expr a -> m) -> Expr a -> m
foldExpr = queryExpr

-- | Map and accumulate over all statements.
foldStmt :: Monoid m => (Stmt a -> m) -> Stmt a -> m
foldStmt q s = q s <> case s of
  StmtBlock _ ss -> foldMap (foldStmt q) ss
  StmtIf _ _ thens elifs mElse ->
    foldMap (foldStmt q) thens <>
    foldMap (\(_, stmts) -> foldMap (foldStmt q) stmts) elifs <>
    maybe mempty (foldMap (foldStmt q)) mElse
  StmtWhile _ _ ss -> foldMap (foldStmt q) ss
  StmtDoWhile _ ss _ -> foldMap (foldStmt q) ss
  StmtFor _ _ _ _ ss -> foldMap (foldStmt q) ss
  StmtForeach _ _ _ _ _ ss -> foldMap (foldStmt q) ss
  StmtSwitch _ _ cases -> foldMap (\case
    SwitchCase _ _ ss -> foldMap (foldStmt q) ss
    SwitchDefault _ ss -> foldMap (foldStmt q) ss) cases
  StmtTry _ tryStmts catches mFinally ->
    foldMap (foldStmt q) tryStmts <>
    foldMap (foldMap (foldStmt q) . catchBody) catches <>
    maybe mempty (foldMap (foldStmt q)) mFinally
  StmtNamespace _ _ mStmts -> maybe mempty (foldMap (foldStmt q)) mStmts
  _ -> mempty


-- | Collect all expressions within an expression.
allExprs :: Expr a -> [Expr a]
allExprs = queryExpr (: [])

-- | Collect all variable names within an expression.
allVariables :: Expr a -> [Text]
allVariables = queryExpr (\case
  ExprVar _ (SimpleVar _ (VarName _ n)) -> [n]
  _ -> [])
