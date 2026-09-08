{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Pretty
  ( -- * High-level printing functions
    prettyPrint
  , prettyPrintStmt
  , prettyPrintExpr
  , prettyPrintType
    -- * Algebraic Document generators
  , prettyProgram
  , prettyStmt
  , prettyExpr
  , prettyType
  , prettyMember
  , prettyParam
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Language.PHP.AST

-- | Render a complete PHP program to formatted Text.
prettyPrint :: Program a -> Text
prettyPrint prog = renderStrict (layoutPretty defaultLayoutOptions (prettyProgram prog))

-- | Render a single statement to formatted Text.
prettyPrintStmt :: Stmt a -> Text
prettyPrintStmt stmt = renderStrict (layoutPretty defaultLayoutOptions (prettyStmt stmt))

-- | Render an expression to formatted Text.
prettyPrintExpr :: Expr a -> Text
prettyPrintExpr expr = renderStrict (layoutPretty defaultLayoutOptions (prettyExpr expr))

-- | Render a type to formatted Text.
prettyPrintType :: Type a -> Text
prettyPrintType typ = renderStrict (layoutPretty defaultLayoutOptions (prettyType typ))

-- | Wadler-Leijen Pretty Document for Program.
prettyProgram :: Program a -> Doc ann
prettyProgram (Program _ stmts) = case stmts of
  StmtInlineHtml _ txt : rest ->
    pretty txt <> if null rest then mempty else "<?php" <> line <> line <> vsep (map prettyStmt rest)
  _ ->
    "<?php" <> line <> line <> vsep (map prettyStmt stmts)

-- | Pretty Document for Statements.
prettyStmt :: Stmt a -> Doc ann
prettyStmt = \case
  StmtExpr _ expr -> prettyExpr expr <> ";"
  StmtBlock _ stmts ->
    "{" <> line <> indent 4 (vsep (map prettyStmt stmts)) <> line <> "}"
  StmtIf _ cond thens elifs mElse ->
    "if (" <> prettyExpr cond <> ") {" <> line <>
    indent 4 (vsep (map prettyStmt thens)) <> line <>
    "}" <> prettyElifs elifs <> prettyElse mElse
  StmtWhile _ cond stmts ->
    "while (" <> prettyExpr cond <> ") {" <> line <>
    indent 4 (vsep (map prettyStmt stmts)) <> line <> "}"
  StmtDoWhile _ stmts cond ->
    "do {" <> line <> indent 4 (vsep (map prettyStmt stmts)) <> line <>
    "} while (" <> prettyExpr cond <> ");"
  StmtFor _ inits conds incrs stmts ->
    "for (" <> hsep (punctuate "," (map prettyExpr inits)) <> ";" <+>
    hsep (punctuate "," (map prettyExpr conds)) <> ";" <+>
    hsep (punctuate "," (map prettyExpr incrs)) <> ") {" <> line <>
    indent 4 (vsep (map prettyStmt stmts)) <> line <> "}"
  StmtForeach _ arr mKey val byRef stmts ->
    "foreach (" <> prettyExpr arr <+> "as" <+>
    maybe mempty (\k -> prettyExpr k <+> "=>" <+> "") mKey <>
    (if byRef then "&" else "") <> prettyExpr val <> ") {" <> line <>
    indent 4 (vsep (map prettyStmt stmts)) <> line <> "}"
  StmtSwitch _ expr cases ->
    "switch (" <> prettyExpr expr <> ") {" <> line <>
    indent 4 (vsep (map prettyCase cases)) <> line <> "}"
  StmtBreak _ mNum ->
    "break" <> maybe mempty (\n -> " " <> prettyExpr n) mNum <> ";"
  StmtContinue _ mNum ->
    "continue" <> maybe mempty (\n -> " " <> prettyExpr n) mNum <> ";"
  StmtReturn _ mExpr ->
    "return" <> maybe mempty (\e -> " " <> prettyExpr e) mExpr <> ";"
  StmtThrowStmt _ expr -> "throw " <> prettyExpr expr <> ";"
  StmtTry _ tryStmts catches mFinally ->
    "try {" <> line <> indent 4 (vsep (map prettyStmt tryStmts)) <> line <> "}" <>
    hcat (map prettyCatch catches) <>
    maybe mempty (\fin -> " finally {" <> line <> indent 4 (vsep (map prettyStmt fin)) <> line <> "}") mFinally
  StmtNamespace _ mName mStmts ->
    "namespace" <> maybe mempty (\n -> " " <> prettyQualifiedName n) mName <>
    case mStmts of
      Nothing -> ";"
      Just ss -> " {" <> line <> indent 4 (vsep (map prettyStmt ss)) <> line <> "}"
  StmtUse _ ut clauses ->
    "use" <> prettyUseType ut <+> hsep (punctuate "," (map prettyUseClause clauses)) <> ";"
  StmtGroupUse _ ut prefix clauses ->
    "use" <> prettyUseType ut <+> prettyQualifiedName prefix <> "\\{" <>
    hsep (punctuate "," (map prettyUseClause clauses)) <> "};"
  StmtConst _ constDecl -> prettyConstDecl constDecl
  StmtFunction _ funcDecl -> prettyFunctionDecl funcDecl
  StmtClass _ classDecl -> prettyClassDecl classDecl
  StmtInterface _ ifaceDecl -> prettyInterfaceDecl ifaceDecl
  StmtTrait _ traitDecl -> prettyTraitDecl traitDecl
  StmtEnum _ enumDecl -> prettyEnumDecl enumDecl
  StmtEcho _ exprs -> "echo " <> hsep (punctuate "," (map prettyExpr exprs)) <> ";"
  StmtGlobal _ vars -> "global " <> hsep (punctuate "," (map prettyExpr vars)) <> ";"
  StmtStatic _ items ->
    "static " <> hsep (punctuate "," (map prettyStaticItem items)) <> ";"
  StmtInlineHtml _ txt -> "?>" <> pretty txt <> "<?php"
  StmtHaltCompiler _ txt -> "__halt_compiler();" <> pretty txt
  StmtEmpty _ -> ";"
  where
    prettyElifs = foldMap (\(c, ss) ->
      " elseif (" <> prettyExpr c <> ") {" <> line <>
      indent 4 (vsep (map prettyStmt ss)) <> line <> "}")
    prettyElse = maybe mempty (\ss ->
      " else {" <> line <> indent 4 (vsep (map prettyStmt ss)) <> line <> "}")
    prettyStaticItem (var, mDef) =
      prettyVarName var <> maybe mempty (\d -> " = " <> prettyExpr d) mDef
    prettyCase = \case
      SwitchCase _ c ss -> "case " <> prettyExpr c <> ":" <> line <> indent 4 (vsep (map prettyStmt ss))
      SwitchDefault _ ss -> "default:" <> line <> indent 4 (vsep (map prettyStmt ss))
    prettyCatch (CatchClause _ types mVar body) =
      " catch (" <> hcat (punctuate "|" (map prettyQualifiedName types)) <>
      maybe mempty (\v -> " " <> prettyVarName v) mVar <> ") {" <> line <>
      indent 4 (vsep (map prettyStmt body)) <> line <> "}"

prettyUseType :: UseType -> Doc ann
prettyUseType = \case
  UseNormal -> ""
  UseFunction -> " function"
  UseConst -> " const"

prettyUseClause :: UseClause a -> Doc ann
prettyUseClause (UseClause _ name mAlias) =
  prettyQualifiedName name <> maybe mempty (\(Ident _ a) -> " as " <> pretty a) mAlias

-- | Class declarations.
prettyClassDecl :: ClassDecl a -> Doc ann
prettyClassDecl (ClassDecl _ attrs modif name mExt impls members) =
  prettyAttributes attrs <>
  (if classFinal modif then "final " else "") <>
  (if classAbstract modif then "abstract " else "") <>
  (if classReadonly modif then "readonly " else "") <>
  "class " <> prettyIdent name <>
  maybe mempty (\ext -> " extends " <> prettyQualifiedName ext) mExt <>
  (if null impls then mempty else " implements " <> hsep (punctuate "," (map prettyQualifiedName impls))) <>
  " {" <> line <> indent 4 (vsep (map prettyMember members)) <> line <> "}"

-- | Interface declarations.
prettyInterfaceDecl :: InterfaceDecl a -> Doc ann
prettyInterfaceDecl (InterfaceDecl _ attrs name extends members) =
  prettyAttributes attrs <>
  "interface " <> prettyIdent name <>
  (if null extends then mempty else " extends " <> hsep (punctuate "," (map prettyQualifiedName extends))) <>
  " {" <> line <> indent 4 (vsep (map prettyMember members)) <> line <> "}"

-- | Trait declarations.
prettyTraitDecl :: TraitDecl a -> Doc ann
prettyTraitDecl (TraitDecl _ attrs name members) =
  prettyAttributes attrs <>
  "trait " <> prettyIdent name <> " {" <> line <>
  indent 4 (vsep (map prettyMember members)) <> line <> "}"

-- | Enum declarations.
prettyEnumDecl :: EnumDecl a -> Doc ann
prettyEnumDecl (EnumDecl _ attrs name mBacked impls members) =
  prettyAttributes attrs <>
  "enum " <> prettyIdent name <>
  maybe mempty (\b -> ": " <> prettyType b) mBacked <>
  (if null impls then mempty else " implements " <> hsep (punctuate "," (map prettyQualifiedName impls))) <>
  " {" <> line <> indent 4 (vsep (map prettyMember members)) <> line <> "}"

-- | Class Member pretty printing.
prettyMember :: ClassMember a -> Doc ann
prettyMember = \case
  MemberConst c -> prettyConstDecl c
  MemberProperty p -> prettyPropertyDecl p
  MemberMethod m -> prettyMethodDecl m
  MemberTraitUse tu -> prettyTraitUse tu
  MemberEnumCase ec -> prettyEnumCase ec

prettyConstDecl :: ConstDecl a -> Doc ann
prettyConstDecl (ConstDecl _ attrs vis isFin mType items) =
  prettyAttributes attrs <>
  maybe mempty (\v -> prettyVisibility v <> " ") vis <>
  (if isFin then "final " else "") <>
  "const " <>
  maybe mempty (\t -> prettyType t <> " ") mType <>
  hsep (punctuate "," (map (\(id', val) -> prettyIdent id' <+> "=" <+> prettyExpr val) items)) <> ";"

prettyPropertyDecl :: PropertyDecl a -> Doc ann
prettyPropertyDecl (PropertyDecl _ attrs modif mType items hooks) =
  prettyAttributes attrs <>
  maybe mempty (\v -> prettyVisibility v <> " ") (propVis modif) <>
  maybe mempty (\wv -> prettyVisibility wv <> "(set) ") (propWriteVis modif) <>
  (if propStatic modif then "static " else "") <>
  (if propReadonly modif then "readonly " else "") <>
  (if propFinal modif then "final " else "") <>
  (if propAbstract modif then "abstract " else "") <>
  maybe mempty (\t -> prettyType t <> " ") mType <>
  hsep (punctuate "," (map (\(var, mVal) -> prettyVarName var <> maybe mempty (\v -> " = " <> prettyExpr v) mVal) items)) <>
  if null hooks
    then ";"
    else " {" <> line <> indent 4 (vsep (map prettyHook hooks)) <> line <> "}"

prettyHook :: PropertyHook a -> Doc ann
prettyHook (PropertyHook _ hookT mParam body) =
  (case hookT of HookGet -> "get"; HookSet -> "set") <>
  maybe mempty (\(var, mTyp) -> "(" <> maybe mempty (\t -> prettyType t <> " ") mTyp <> prettyVarName var <> ")") mParam <>
  case body of
    HookExpr e -> " => " <> prettyExpr e <> ";"
    HookBlock ss -> " {" <> line <> indent 4 (vsep (map prettyStmt ss)) <> line <> "}"
    HookAbstract -> ";"

prettyMethodDecl :: MethodDecl a -> Doc ann
prettyMethodDecl (MethodDecl _ attrs modif byRef name params retType body) =
  prettyAttributes attrs <>
  maybe mempty (\v -> prettyVisibility v <> " ") (methodVis modif) <>
  (if methodStatic modif then "static " else "") <>
  (if methodFinal modif then "final " else "") <>
  (if methodAbstract modif then "abstract " else "") <>
  "function " <> (if byRef then "&" else "") <> prettyIdent name <>
  "(" <> hsep (punctuate "," (map prettyParam params)) <> ")" <>
  maybe mempty (\r -> ": " <> prettyType r) retType <>
  case body of
    Nothing -> ";"
    Just ss -> " {" <> line <> indent 4 (vsep (map prettyStmt ss)) <> line <> "}"

prettyParam :: Param a -> Doc ann
prettyParam (Param _ attrs vis wVis isRo mType byRef isVariadic name mDef) =
  prettyAttributes attrs <>
  maybe mempty (\v -> prettyVisibility v <> " ") vis <>
  maybe mempty (\wv -> prettyVisibility wv <> "(set) ") wVis <>
  (if isRo then "readonly " else "") <>
  maybe mempty (\t -> prettyType t <> " ") mType <>
  (if byRef then "&" else "") <>
  (if isVariadic then "..." else "") <>
  prettyVarName name <>
  maybe mempty (\d -> " = " <> prettyExpr d) mDef

prettyFunctionDecl :: FunctionDecl a -> Doc ann
prettyFunctionDecl (FunctionDecl _ attrs byRef name params retType body) =
  prettyAttributes attrs <>
  "function " <> (if byRef then "&" else "") <> prettyIdent name <>
  "(" <> hsep (punctuate "," (map prettyParam params)) <> ")" <>
  maybe mempty (\r -> ": " <> prettyType r) retType <>
  " {" <> line <> indent 4 (vsep (map prettyStmt body)) <> line <> "}"

prettyTraitUse :: TraitUse a -> Doc ann
prettyTraitUse (TraitUse _ names adapts) =
  "use " <> hsep (punctuate "," (map prettyQualifiedName names)) <>
  if null adapts
    then ";"
    else " {" <> line <> indent 4 (vsep (map prettyAdaptation adapts)) <> line <> "}"
  where
    prettyAdaptation = \case
      TraitPrecedence _ tr met others ->
        prettyQualifiedName tr <> "::" <> prettyIdent met <+> "insteadof " <>
        hsep (punctuate "," (map prettyQualifiedName others)) <> ";"
      TraitAlias _ mTr met vis mNew ->
        maybe mempty (\tr -> prettyQualifiedName tr <> "::") mTr <>
        prettyIdent met <+> "as" <>
        maybe mempty (\v -> " " <> prettyVisibility v) vis <>
        maybe mempty (\n -> " " <> prettyIdent n) mNew <> ";"

prettyEnumCase :: EnumCase a -> Doc ann
prettyEnumCase (EnumCase _ attrs name mVal) =
  prettyAttributes attrs <> "case " <> prettyIdent name <>
  maybe mempty (\v -> " = " <> prettyExpr v) mVal <> ";"

prettyVisibility :: Visibility -> Doc ann
prettyVisibility = \case
  Public -> "public"
  Protected -> "protected"
  Private -> "private"

prettyAttributes :: [AttributeGroup a] -> Doc ann
prettyAttributes = foldMap (\(AttributeGroup _ attrs) ->
  "#[" <> hsep (punctuate "," (map prettyAttr attrs)) <> "]" <> line)
  where
    prettyAttr (Attribute _ name args) =
      prettyQualifiedName name <>
      if null args
        then mempty
        else "(" <> hsep (punctuate "," (map prettyArg args)) <> ")"

prettyArg :: Arg a -> Doc ann
prettyArg (Arg _ mName expr isUnpack) =
  maybe mempty (\(Ident _ n) -> pretty n <> ": ") mName <>
  (if isUnpack then "..." else "") <>
  prettyExpr expr

-- | Pretty document for PHP Expressions.
prettyExpr :: Expr a -> Doc ann
prettyExpr = \case
  ExprVar _ v -> prettyVar v
  ExprLit _ l -> prettyLiteral l
  ExprBinary _ op lhs rhs ->
    parens (prettySubExpr lhs <+> prettyBinOp op <+> prettySubExpr rhs)
  ExprUnary _ op e -> prettyUnary op e
  ExprAssign _ mOp lhs rhs ->
    prettySubExpr lhs <+> prettyAssignOp mOp <+> prettySubExpr rhs
  ExprTernary _ cond (Just t) f ->
    parens (prettySubExpr cond <+> "?" <+> prettySubExpr t <+> ":" <+> prettySubExpr f)
  ExprTernary _ cond Nothing f ->
    parens (prettySubExpr cond <+> "?:" <+> prettySubExpr f)
  ExprNullCoalesce _ lhs rhs ->
    parens (prettySubExpr lhs <+> "??" <+> prettySubExpr rhs)
  ExprClone _ obj Nothing -> "clone " <> prettySubExpr obj
  ExprClone _ obj (Just with) ->
    "clone(" <> prettySubExpr obj <> ", [" <>
    hsep (punctuate "," (map (\(k, v) -> prettySubExpr k <+> "=>" <+> prettySubExpr v) with)) <> "])"
  ExprNew _ target args ->
    "new " <> prettyNewTarget target <> "(" <> hsep (punctuate "," (map prettyArg args)) <> ")"
  ExprNewAnonClass _ attrs modif args ext impls members ->
    prettyAttributes attrs <>
    "new " <> (if classReadonly modif then "readonly " else "") <> "class(" <>
    hsep (punctuate "," (map prettyArg args)) <> ")" <>
    maybe mempty (\e -> " extends " <> prettyQualifiedName e) ext <>
    (if null impls then mempty else " implements " <> hsep (punctuate "," (map prettyQualifiedName impls))) <>
    " {" <> line <> indent 4 (vsep (map prettyMember members)) <> line <> "}"
  ExprCall _ fn args -> prettyCallBase fn <> prettyCallArgs args
  ExprMethodCall _ obj member args ->
    prettyPostfixBase obj <> "->" <> prettyMemberName member <> prettyCallArgs args
  ExprNullsafeMethodCall _ obj member args ->
    prettyPostfixBase obj <> "?->" <> prettyMemberName member <> prettyCallArgs args
  ExprPropertyFetch _ obj member ->
    prettyPostfixBase obj <> "->" <> prettyMemberName member
  ExprNullsafePropertyFetch _ obj member ->
    prettyPostfixBase obj <> "?->" <> prettyMemberName member
  ExprStaticCall _ target member args ->
    prettyTarget target <> "::" <> prettyMemberName member <> prettyCallArgs args
  ExprStaticPropertyFetch _ target var ->
    prettyTarget target <> "::" <> prettyVarName var
  ExprClassConstFetch _ target constName ->
    prettyTarget target <> "::" <> prettyConstName constName
  ExprArray _ items ->
    "[" <> hsep (punctuate "," (map prettyArrayItem items)) <> "]"
  ExprArrayAccess _ arr mIdx ->
    prettyPostfixBase arr <> "[" <> maybe mempty prettyExpr mIdx <> "]"
  ExprMatch _ subject arms ->
    "match (" <> prettyExpr subject <> ") {" <> line <>
    indent 4 (vsep (punctuate "," (map prettyMatchArm arms))) <> line <> "}"
  ExprClosure _ attrs byRef isStat params uses retType stmts ->
    prettyAttributes attrs <>
    (if isStat then "static " else "") <> "function " <> (if byRef then "&" else "") <>
    "(" <> hsep (punctuate "," (map prettyParam params)) <> ")" <>
    (if null uses then mempty else " use (" <> hsep (punctuate "," (map (\(v, r) -> (if r then "&" else "") <> prettyVarName v) uses)) <> ")") <>
    maybe mempty (\r -> ": " <> prettyType r) retType <>
    " {" <> line <> indent 4 (vsep (map prettyStmt stmts)) <> line <> "}"
  ExprArrowFunction _ attrs byRef isStat params retType expr ->
    prettyAttributes attrs <>
    (if isStat then "static " else "") <> "fn " <> (if byRef then "&" else "") <>
    "(" <> hsep (punctuate "," (map prettyParam params)) <> ")" <>
    maybe mempty (\r -> ": " <> prettyType r) retType <+> "=> " <> prettyExpr expr
  ExprYield _ mK mV ->
    "yield" <> maybe mempty (\k -> " " <> prettyExpr k <+> "=>") mK <>
    maybe mempty (\v -> " " <> prettyExpr v) mV
  ExprYieldFrom _ e -> "yield from " <> prettyExpr e
  ExprCast _ ct e -> "(" <> prettyCastType ct <> ")" <> prettySubExpr e
  ExprIsset _ es -> "isset(" <> hsep (punctuate "," (map prettyExpr es)) <> ")"
  ExprEmpty _ e -> "empty(" <> prettyExpr e <> ")"
  ExprEval _ e -> "eval(" <> prettyExpr e <> ")"
  ExprInclude _ inc e -> prettyInclude inc <+> prettyExpr e
  ExprThrow _ e -> "throw " <> prettyExpr e
  ExprConstFetch _ qn -> prettyQualifiedName qn

-- | Render an expression in an operator-operand position. Assignment
-- binds more loosely than every operator that can enclose it, so an
-- operand-position assignment must be parenthesized to survive re-parsing.
prettySubExpr :: Expr a -> Doc ann
prettySubExpr e
  | needsAssignParens e = parens (prettyExpr e)
  | otherwise           = prettyExpr e

needsAssignParens :: Expr a -> Bool
needsAssignParens = \case
  ExprAssign {} -> True
  _             -> False

prettyBinOp :: BinOp -> Doc ann
prettyBinOp = \case
  OpAdd -> "+"
  OpSub -> "-"
  OpMul -> "*"
  OpDiv -> "/"
  OpMod -> "%"
  OpPow -> "**"
  OpConcat -> "."
  OpBitAnd -> "&"
  OpBitOr -> "|"
  OpBitXor -> "^"
  OpShiftLeft -> "<<"
  OpShiftRight -> ">>"
  OpEq -> "=="
  OpIdentical -> "==="
  OpNotEq -> "!="
  OpNotIdentical -> "!=="
  OpLt -> "<"
  OpLte -> "<="
  OpGt -> ">"
  OpGte -> ">="
  OpSpaceship -> "<=>"
  OpBoolAnd -> "&&"
  OpBoolOr -> "||"
  OpLogicalAnd -> "and"
  OpLogicalOr -> "or"
  OpLogicalXor -> "xor"
  OpInstanceof -> "instanceof"
  OpPipe -> "|>"
  OpCoalesce -> "??"

prettyUnary :: UnOp -> Expr a -> Doc ann
prettyUnary op e = case op of
  OpPreInc -> "++" <> prettySubExpr e
  OpPostInc -> prettySubExpr e <> "++"
  OpPreDec -> "--" <> prettySubExpr e
  OpPostDec -> prettySubExpr e <> "--"
  OpUnaryPlus ->
    let spaceSep = case e of
          ExprUnary _ OpUnaryPlus _ -> " "
          ExprUnary _ OpPreInc _    -> " "
          _                         -> mempty
    in "+" <> spaceSep <> prettySubExpr e
  OpUnaryMinus ->
    let spaceSep = case e of
          ExprUnary _ OpUnaryMinus _ -> " "
          ExprUnary _ OpPreDec _     -> " "
          _                          -> mempty
    in "-" <> spaceSep <> prettySubExpr e
  OpBoolNot -> "!" <> prettySubExpr e
  OpBitNot -> "~" <> prettySubExpr e
  OpErrorSuppress -> "@" <> prettySubExpr e

prettyAssignOp :: Maybe BinOp -> Doc ann
prettyAssignOp = \case
  Nothing -> "="
  Just op -> prettyBinOp op <> "="

prettyCallArgs :: CallArgs a -> Doc ann
prettyCallArgs = \case
  ArgsList args -> "(" <> hsep (punctuate "," (map prettyArg args)) <> ")"
  FirstClassCallable -> "(...)"

prettyCallBase :: Expr a -> Doc ann
prettyCallBase e
  | needsCallParens e = parens (prettyExpr e)
  | otherwise         = prettyPostfixBase e

needsCallParens :: Expr a -> Bool
needsCallParens = \case
  ExprPropertyFetch {}         -> True
  ExprNullsafePropertyFetch {} -> True
  ExprStaticPropertyFetch {}   -> True
  ExprClassConstFetch {}       -> True
  _                            -> False

prettyPostfixBase :: Expr a -> Doc ann
prettyPostfixBase e
  | needsPostfixParens e = parens (prettyExpr e)
  | otherwise            = prettyExpr e

needsPostfixParens :: Expr a -> Bool
needsPostfixParens = \case
  ExprCast {}   -> True
  ExprUnary {}  -> True
  ExprClone {}  -> True
  ExprAssign {} -> True
  _             -> False

prettyNewTarget :: ClassTarget a -> Doc ann
prettyNewTarget = \case
  ClassTargetName qn -> prettyQualifiedName qn
  ClassTargetExpr e
    | isDynamicTarget e -> prettyExpr e
    | otherwise         -> parens (prettyExpr e)
  where
    isDynamicTarget = \case
      ExprVar {}                   -> True
      ExprPropertyFetch {}         -> True
      ExprNullsafePropertyFetch {}  -> True
      ExprArrayAccess {}           -> True
      ExprStaticPropertyFetch {}   -> True
      _                            -> False

prettyTarget :: ClassTarget a -> Doc ann
prettyTarget = \case
  ClassTargetName qn -> prettyQualifiedName qn
  ClassTargetExpr e -> case e of
    ExprVar {} -> prettyExpr e
    _          -> parens (prettyExpr e)

prettyMemberName :: MemberName a -> Doc ann
prettyMemberName = \case
  MemberIdent id' -> prettyIdent id'
  MemberExpr e -> case e of
    ExprVar {} -> prettyExpr e
    _          -> "{" <> prettyExpr e <> "}"

prettyConstName :: ClassConstName a -> Doc ann
prettyConstName = \case
  ConstNameIdent id' -> prettyIdent id'
  ConstNameDynamic e -> "{" <> prettyExpr e <> "}"

prettyArrayItem :: ArrayItem a -> Doc ann
prettyArrayItem (ArrayItem _ mKey val isUnpack) =
  maybe mempty (\k -> prettyExpr k <+> "=> ") mKey <>
  (if isUnpack then "..." else "") <>
  prettyExpr val

prettyMatchArm :: MatchArm a -> Doc ann
prettyMatchArm = \case
  MatchArm _ conds res ->
    hsep (punctuate "," (map prettyExpr conds)) <+> "=> " <> prettyExpr res
  MatchDefault _ res -> "default => " <> prettyExpr res

prettyLiteral :: Literal a -> Doc ann
prettyLiteral = \case
  LitInt _ _ raw -> pretty raw
  LitFloat _ _ raw -> pretty raw
  LitString _ _ raw -> pretty raw
  LitInterpolated _ parts -> "\"" <> foldMap prettyPart parts <> "\""
  LitHeredoc _ tag content False -> "<<<" <> pretty tag <> line <> pretty content <> line <> pretty tag
  LitHeredoc _ tag content True -> "<<<'" <> pretty tag <> "'" <> line <> pretty content <> line <> pretty tag
  LitBool _ True -> "true"
  LitBool _ False -> "false"
  LitNull _ -> "null"
  where
    prettyPart = \case
      StrLit t -> pretty t
      StrExpr e -> "{" <> prettyExpr e <> "}"

prettyVar :: Var a -> Doc ann
prettyVar = \case
  SimpleVar _ vn -> prettyVarName vn
  DynamicVar _ (ExprVar _ innerVar) -> "$" <> prettyVar innerVar
  DynamicVar _ e -> "${" <> prettyExpr e <> "}"

prettyCastType :: CastType -> Doc ann
prettyCastType = \case
  CastInt -> "int"
  CastFloat -> "float"
  CastString -> "string"
  CastBool -> "bool"
  CastArray -> "array"
  CastObject -> "object"
  CastUnset -> "unset"

prettyInclude :: IncludeType -> Doc ann
prettyInclude = \case
  IncInclude -> "include"
  IncIncludeOnce -> "include_once"
  IncRequire -> "require"
  IncRequireOnce -> "require_once"

prettyIdent :: Ident a -> Doc ann
prettyIdent (Ident _ name) = pretty name

prettyVarName :: VarName a -> Doc ann
prettyVarName (VarName _ name) = "$" <> pretty name

prettyQualifiedName :: QualifiedName a -> Doc ann
prettyQualifiedName (QualifiedName _ kind parts) =
  case kind of
    NameFullyQualified -> "\\" <> pretty (T.intercalate "\\" parts)
    NameRelative -> "namespace\\" <> pretty (T.intercalate "\\" parts)
    _ -> pretty (T.intercalate "\\" parts)

-- | Pretty document for PHP Types (supports DNF, unions, intersections).
prettyType :: Type a -> Doc ann
prettyType = \case
  SimpleType _ qn -> prettyQualifiedName qn
  NullableType _ t -> "?" <> prettyType t
  UnionType _ ts -> hcat (punctuate "|" (map prettyType ts))
  IntersectionType _ ts -> hcat (punctuate "&" (map prettyType ts))
  DNFType _ ts -> parens (hcat (punctuate "&" (map prettyType ts)))
