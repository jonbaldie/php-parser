{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , HasLeadingTrivia (..)
  ) where

import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Language.PHP.AST

-- | An annotation may carry comments that precede the annotated node.
--
-- The fallback instance keeps the pretty-printer source-compatible with the
-- existing API, whose AST annotations are intentionally polymorphic.
class HasLeadingTrivia a where
  leadingTrivia :: a -> [Trivia]

instance HasLeadingTrivia (Annotated a) where
  leadingTrivia = annTrivia

instance {-# OVERLAPPABLE #-} HasLeadingTrivia a where
  leadingTrivia _ = []

prettyLeadingTrivia :: HasLeadingTrivia a => a -> Doc ann -> Doc ann
prettyLeadingTrivia annotation doc = case leadingTrivia annotation of
  [] -> doc
  trivia -> vsep (map prettyTrivia trivia ++ [doc])

prettyTrivia :: Trivia -> Doc ann
prettyTrivia = \case
  CommentLine txt -> "//" <> pretty txt
  CommentBlock txt -> "/*" <> pretty txt <> "*/"
  DocBlock txt -> "/**" <> pretty txt <> "*/"

-- | Render a complete PHP program to formatted Text.
prettyPrint :: HasLeadingTrivia a => Program a -> Text
prettyPrint prog = renderStrict (layoutPretty defaultLayoutOptions (prettyProgram prog))

-- | Render a single statement to formatted Text.
prettyPrintStmt :: HasLeadingTrivia a => Stmt a -> Text
prettyPrintStmt stmt = renderStrict (layoutPretty defaultLayoutOptions (prettyStmt stmt))

-- | Render an expression to formatted Text.
prettyPrintExpr :: HasLeadingTrivia a => Expr a -> Text
prettyPrintExpr expr = renderStrict (layoutPretty defaultLayoutOptions (prettyExpr expr))

-- | Render a type to formatted Text.
prettyPrintType :: HasLeadingTrivia a => Type a -> Text
prettyPrintType typ = renderStrict (layoutPretty defaultLayoutOptions (prettyType typ))

-- | Wadler-Leijen Pretty Document for Program.
prettyProgram :: HasLeadingTrivia a => Program a -> Doc ann
prettyProgram (Program annotation stmts) = prettyLeadingTrivia annotation $ case stmts of
  StmtInlineHtml _ txt : rest ->
    pretty txt <> if null rest then mempty else "<?php" <> line <> line <> vsep (map prettyStmt rest)
  _ ->
    "<?php" <> line <> line <> vsep (map prettyStmt stmts)

-- | Pretty Document for Statements.
prettyStmt :: HasLeadingTrivia a => Stmt a -> Doc ann
prettyStmt stmt = prettyLeadingTrivia (stmtAnnotation stmt) $ case stmt of
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
  StmtDeclare _ dirs mBody ->
    "declare(" <> hsep (punctuate "," (map prettyDeclareDirective dirs)) <> ")" <>
    case mBody of
      Nothing -> ";"
      Just ss -> " {" <> line <> indent 4 (vsep (map prettyStmt ss)) <> line <> "}"
  StmtGoto _ label -> "goto " <> prettyIdent label <> ";"
  StmtLabel _ label -> prettyIdent label <> ":"
  StmtUnset _ targets -> "unset(" <> hsep (punctuate "," (map prettyExpr targets)) <> ");"
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
    prettyCase switchCase = prettyLeadingTrivia (switchCaseAnnotation switchCase) $ case switchCase of
      SwitchCase _ c ss -> "case " <> prettyExpr c <> ":" <> line <> indent 4 (vsep (map prettyStmt ss))
      SwitchDefault _ ss -> "default:" <> line <> indent 4 (vsep (map prettyStmt ss))
    prettyCatch catchClause = prettyLeadingTrivia (catchAnnotation catchClause) $ case catchClause of
      CatchClause _ types mVar body ->
        " catch (" <> hcat (punctuate "|" (map prettyQualifiedName types)) <>
        maybe mempty (\v -> " " <> prettyVarName v) mVar <> ") {" <> line <>
        indent 4 (vsep (map prettyStmt body)) <> line <> "}"

    switchCaseAnnotation = \case
      SwitchCase annotation _ _ -> annotation
      SwitchDefault annotation _ -> annotation
    catchAnnotation (CatchClause annotation _ _ _) = annotation

prettyDeclareDirective :: HasLeadingTrivia a => DeclareDirective a -> Doc ann
prettyDeclareDirective (DeclareDirective annotation name val) =
  prettyLeadingTrivia annotation $
    prettyIdent name <> "=" <> prettyLiteral val

stmtAnnotation :: Stmt a -> a
stmtAnnotation = \case
  StmtExpr a _ -> a
  StmtBlock a _ -> a
  StmtIf a _ _ _ _ -> a
  StmtWhile a _ _ -> a
  StmtDoWhile a _ _ -> a
  StmtFor a _ _ _ _ -> a
  StmtForeach a _ _ _ _ _ -> a
  StmtSwitch a _ _ -> a
  StmtBreak a _ -> a
  StmtContinue a _ -> a
  StmtReturn a _ -> a
  StmtThrowStmt a _ -> a
  StmtTry a _ _ _ -> a
  StmtNamespace a _ _ -> a
  StmtUse a _ _ -> a
  StmtGroupUse a _ _ _ -> a
  StmtConst a _ -> a
  StmtFunction a _ -> a
  StmtClass a _ -> a
  StmtInterface a _ -> a
  StmtTrait a _ -> a
  StmtEnum a _ -> a
  StmtEcho a _ -> a
  StmtGlobal a _ -> a
  StmtStatic a _ -> a
  StmtDeclare a _ _ -> a
  StmtGoto a _ -> a
  StmtLabel a _ -> a
  StmtUnset a _ -> a
  StmtInlineHtml a _ -> a
  StmtHaltCompiler a _ -> a
  StmtEmpty a -> a

prettyUseType :: UseType -> Doc ann
prettyUseType = \case
  UseNormal -> ""
  UseFunction -> " function"
  UseConst -> " const"

prettyUseClause :: HasLeadingTrivia a => UseClause a -> Doc ann
prettyUseClause (UseClause annotation name mAlias) = prettyLeadingTrivia annotation $
  prettyQualifiedName name <> maybe mempty (\alias -> " as " <> prettyIdent alias) mAlias

-- | Class declarations.
prettyClassDecl :: HasLeadingTrivia a => ClassDecl a -> Doc ann
prettyClassDecl (ClassDecl annotation attrs modif name mExt impls members) = prettyLeadingTrivia annotation $
  prettyAttributes attrs <>
  (if classFinal modif then "final " else "") <>
  (if classAbstract modif then "abstract " else "") <>
  (if classReadonly modif then "readonly " else "") <>
  "class " <> prettyIdent name <>
  maybe mempty (\ext -> " extends " <> prettyQualifiedName ext) mExt <>
  (if null impls then mempty else " implements " <> hsep (punctuate "," (map prettyQualifiedName impls))) <>
  " {" <> line <> indent 4 (vsep (map prettyMember members)) <> line <> "}"

-- | Interface declarations.
prettyInterfaceDecl :: HasLeadingTrivia a => InterfaceDecl a -> Doc ann
prettyInterfaceDecl (InterfaceDecl annotation attrs name extends members) = prettyLeadingTrivia annotation $
  prettyAttributes attrs <>
  "interface " <> prettyIdent name <>
  (if null extends then mempty else " extends " <> hsep (punctuate "," (map prettyQualifiedName extends))) <>
  " {" <> line <> indent 4 (vsep (map prettyMember members)) <> line <> "}"

-- | Trait declarations.
prettyTraitDecl :: HasLeadingTrivia a => TraitDecl a -> Doc ann
prettyTraitDecl (TraitDecl annotation attrs name members) = prettyLeadingTrivia annotation $
  prettyAttributes attrs <>
  "trait " <> prettyIdent name <> " {" <> line <>
  indent 4 (vsep (map prettyMember members)) <> line <> "}"

-- | Enum declarations.
prettyEnumDecl :: HasLeadingTrivia a => EnumDecl a -> Doc ann
prettyEnumDecl (EnumDecl annotation attrs name mBacked impls members) = prettyLeadingTrivia annotation $
  prettyAttributes attrs <>
  "enum " <> prettyIdent name <>
  maybe mempty (\b -> ": " <> prettyType b) mBacked <>
  (if null impls then mempty else " implements " <> hsep (punctuate "," (map prettyQualifiedName impls))) <>
  " {" <> line <> indent 4 (vsep (map prettyMember members)) <> line <> "}"

-- | Class Member pretty printing.
prettyMember :: HasLeadingTrivia a => ClassMember a -> Doc ann
prettyMember = \case
  MemberConst c -> prettyConstDecl c
  MemberProperty p -> prettyPropertyDecl p
  MemberMethod m -> prettyMethodDecl m
  MemberTraitUse tu -> prettyTraitUse tu
  MemberEnumCase ec -> prettyEnumCase ec

prettyConstDecl :: HasLeadingTrivia a => ConstDecl a -> Doc ann
prettyConstDecl (ConstDecl annotation attrs vis isFin mType items) = prettyLeadingTrivia annotation $
  prettyAttributes attrs <>
  maybe mempty (\v -> prettyVisibility v <> " ") vis <>
  (if isFin then "final " else "") <>
  "const " <>
  maybe mempty (\t -> prettyType t <> " ") mType <>
  hsep (punctuate "," (map (\(id', val) -> prettyIdent id' <+> "=" <+> prettyExpr val) items)) <> ";"

prettyPropertyDecl :: HasLeadingTrivia a => PropertyDecl a -> Doc ann
prettyPropertyDecl (PropertyDecl annotation attrs modif mType items hooks) = prettyLeadingTrivia annotation $
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

prettyHook :: HasLeadingTrivia a => PropertyHook a -> Doc ann
prettyHook (PropertyHook annotation isFinal hookT mParam body) = prettyLeadingTrivia annotation $
  (if isFinal then "final " else "") <>
  (case hookT of HookGet -> "get"; HookSet -> "set") <>
  maybe mempty (\(var, mTyp) -> "(" <> maybe mempty (\t -> prettyType t <> " ") mTyp <> prettyVarName var <> ")") mParam <>
  case body of
    HookExpr e -> " => " <> prettyExpr e <> ";"
    HookBlock ss -> " {" <> line <> indent 4 (vsep (map prettyStmt ss)) <> line <> "}"
    HookAbstract -> ";"

prettyMethodDecl :: HasLeadingTrivia a => MethodDecl a -> Doc ann
prettyMethodDecl (MethodDecl annotation attrs modif byRef name params retType body) = prettyLeadingTrivia annotation $
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

prettyParam :: HasLeadingTrivia a => Param a -> Doc ann
prettyParam (Param annotation attrs vis wVis isRo mType byRef isVariadic name mDef) = prettyLeadingTrivia annotation $
  prettyAttributes attrs <>
  maybe mempty (\v -> prettyVisibility v <> " ") vis <>
  maybe mempty (\wv -> prettyVisibility wv <> "(set) ") wVis <>
  (if isRo then "readonly " else "") <>
  maybe mempty (\t -> prettyType t <> " ") mType <>
  (if byRef then "&" else "") <>
  (if isVariadic then "..." else "") <>
  prettyVarName name <>
  maybe mempty (\d -> " = " <> prettyExpr d) mDef

prettyFunctionDecl :: HasLeadingTrivia a => FunctionDecl a -> Doc ann
prettyFunctionDecl (FunctionDecl annotation attrs byRef name params retType body) = prettyLeadingTrivia annotation $
  prettyAttributes attrs <>
  "function " <> (if byRef then "&" else "") <> prettyIdent name <>
  "(" <> hsep (punctuate "," (map prettyParam params)) <> ")" <>
  maybe mempty (\r -> ": " <> prettyType r) retType <>
  " {" <> line <> indent 4 (vsep (map prettyStmt body)) <> line <> "}"

prettyTraitUse :: HasLeadingTrivia a => TraitUse a -> Doc ann
prettyTraitUse (TraitUse annotation names adapts) = prettyLeadingTrivia annotation $
  "use " <> hsep (punctuate "," (map prettyQualifiedName names)) <>
  if null adapts
    then ";"
    else " {" <> line <> indent 4 (vsep (map prettyAdaptation adapts)) <> line <> "}"
  where
    prettyAdaptation adaptation = prettyLeadingTrivia (traitAdaptationAnnotation adaptation) $ case adaptation of
      TraitPrecedence _ tr met others ->
        prettyQualifiedName tr <> "::" <> prettyIdent met <+> "insteadof " <>
        hsep (punctuate "," (map prettyQualifiedName others)) <> ";"
      TraitAlias _ mTr met vis mNew ->
        maybe mempty (\tr -> prettyQualifiedName tr <> "::") mTr <>
        prettyIdent met <+> "as" <>
        maybe mempty (\v -> " " <> prettyVisibility v) vis <>
        maybe mempty (\n -> " " <> prettyIdent n) mNew <> ";"

    traitAdaptationAnnotation = \case
      TraitPrecedence triviaAnn _ _ _ -> triviaAnn
      TraitAlias triviaAnn _ _ _ _ -> triviaAnn

prettyEnumCase :: HasLeadingTrivia a => EnumCase a -> Doc ann
prettyEnumCase (EnumCase annotation attrs name mVal) = prettyLeadingTrivia annotation $
  prettyAttributes attrs <> "case " <> prettyIdent name <>
  maybe mempty (\v -> " = " <> prettyExpr v) mVal <> ";"

prettyVisibility :: Visibility -> Doc ann
prettyVisibility = \case
  Public -> "public"
  Protected -> "protected"
  Private -> "private"

prettyAttributes :: HasLeadingTrivia a => [AttributeGroup a] -> Doc ann
prettyAttributes = foldMap (\attrs -> prettyAttributeGroup attrs <> line)

prettyAttributesInline :: HasLeadingTrivia a => [AttributeGroup a] -> Doc ann
prettyAttributesInline = hsep . map prettyAttributeGroup

prettyAttributeGroup :: HasLeadingTrivia a => AttributeGroup a -> Doc ann
prettyAttributeGroup (AttributeGroup annotation attrs) = prettyLeadingTrivia annotation $
  "#[" <> hsep (punctuate "," (map prettyAttr attrs)) <> "]"
  where
    prettyAttr (Attribute attrAnnotation name args) = prettyLeadingTrivia attrAnnotation $
      prettyQualifiedName name <>
      if null args
        then mempty
        else "(" <> hsep (punctuate "," (map prettyArg args)) <> ")"

prettyArg :: HasLeadingTrivia a => Arg a -> Doc ann
prettyArg (Arg annotation mName expr isUnpack) = prettyLeadingTrivia annotation $
  maybe mempty (\name -> prettyIdent name <> ": ") mName <>
  (if isUnpack then "..." else "") <>
  prettyExpr expr

-- | Pretty document for PHP Expressions.
prettyExpr :: HasLeadingTrivia a => Expr a -> Doc ann
prettyExpr expr = prettyLeadingTrivia (getAnnotation expr) $ case expr of
  ExprVar _ v -> prettyVar v
  ExprLit _ l -> prettyLiteral l
  ExprBinary _ OpPow lhs rhs ->
    parens (prettyPowLhs lhs <+> prettyBinOp OpPow <+> prettySubExpr rhs)
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
    "new " <> prettyAttributesInline attrs <>
    (if classReadonly modif then "readonly " else "") <> "class(" <>
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
  ExprArrowFunction _ attrs byRef isStat params retType bodyExpr ->
    prettyAttributes attrs <>
    (if isStat then "static " else "") <> "fn " <> (if byRef then "&" else "") <>
    "(" <> hsep (punctuate "," (map prettyParam params)) <> ")" <>
    maybe mempty (\r -> ": " <> prettyType r) retType <+> "=> " <> prettyExpr bodyExpr
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
prettySubExpr :: HasLeadingTrivia a => Expr a -> Doc ann
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

prettyUnary :: HasLeadingTrivia a => UnOp -> Expr a -> Doc ann
prettyUnary op e = case op of
  -- Postfix ++/-- bind tighter than the prefix operators above them, so
  -- the operand takes the same parenthesization as every other postfix
  -- base (e.g. ((int)$x)++, else (int)$x++ reparses as (int)($x++)).
  OpPreInc -> "++" <> prettySubExpr e
  OpPostInc -> prettyPostfixBase e <> "++"
  OpPreDec -> "--" <> prettySubExpr e
  OpPostDec -> prettyPostfixBase e <> "--"
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

-- | Render the left operand of "**". Exponentiation binds tighter than the
-- prefix operators and casts, so an unparenthesized "-2 ** 2" reparses as
-- "-(2 ** 2)"; such an operand must keep its own parentheses.
prettyPowLhs :: HasLeadingTrivia a => Expr a -> Doc ann
prettyPowLhs e
  | bindsLooserThanPow e = parens (prettyExpr e)
  | otherwise            = prettySubExpr e
  where
    bindsLooserThanPow = \case
      ExprCast {}                    -> True
      ExprUnary _ OpUnaryPlus _      -> True
      ExprUnary _ OpUnaryMinus _     -> True
      ExprUnary _ OpBitNot _         -> True
      ExprUnary _ OpBoolNot _        -> True
      ExprUnary _ OpErrorSuppress _  -> True
      _                              -> False

prettyAssignOp :: Maybe BinOp -> Doc ann
prettyAssignOp = \case
  Nothing -> "="
  Just op -> prettyBinOp op <> "="

prettyCallArgs :: HasLeadingTrivia a => CallArgs a -> Doc ann
prettyCallArgs = \case
  ArgsList args -> "(" <> hsep (punctuate "," (map prettyArg args)) <> ")"
  FirstClassCallable -> "(...)"

prettyCallBase :: HasLeadingTrivia a => Expr a -> Doc ann
prettyCallBase e
  | needsCallParens e = parens (prettyExpr e)
  | otherwise         = prettyPostfixBase e

needsCallParens :: Expr a -> Bool
needsCallParens = \case
  ExprPropertyFetch {}         -> True
  ExprNullsafePropertyFetch {} -> True
  ExprStaticPropertyFetch {}   -> True
  ExprClassConstFetch {}       -> True
  ExprYield {}                 -> True
  ExprYieldFrom {}             -> True
  ExprArrowFunction {}         -> True
  ExprThrow {}                 -> True
  _                            -> False

prettyPostfixBase :: HasLeadingTrivia a => Expr a -> Doc ann
prettyPostfixBase e
  | needsPostfixParens e = parens (prettyExpr e)
  | otherwise            = prettyExpr e

needsPostfixParens :: Expr a -> Bool
needsPostfixParens = \case
  ExprCast {}          -> True
  ExprUnary {}         -> True
  ExprClone {}         -> True
  ExprAssign {}        -> True
  ExprYield {}         -> True
  ExprYieldFrom {}     -> True
  ExprArrowFunction {} -> True
  ExprThrow {}         -> True
  ExprInclude {}       -> True
  _                    -> False

prettyNewTarget :: HasLeadingTrivia a => ClassTarget a -> Doc ann
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

prettyTarget :: HasLeadingTrivia a => ClassTarget a -> Doc ann
prettyTarget = \case
  ClassTargetName qn -> prettyQualifiedName qn
  ClassTargetExpr e -> case e of
    ExprVar {} -> prettyExpr e
    _          -> parens (prettyExpr e)

prettyMemberName :: HasLeadingTrivia a => MemberName a -> Doc ann
prettyMemberName = \case
  MemberIdent id' -> prettyIdent id'
  MemberExpr e -> case e of
    ExprVar {} -> prettyExpr e
    _          -> "{" <> prettyExpr e <> "}"

prettyConstName :: HasLeadingTrivia a => ClassConstName a -> Doc ann
prettyConstName = \case
  ConstNameIdent id' -> prettyIdent id'
  ConstNameDynamic e -> "{" <> prettyExpr e <> "}"

prettyArrayItem :: HasLeadingTrivia a => ArrayItem a -> Doc ann
prettyArrayItem (ArrayItem annotation mKey val isUnpack) = prettyLeadingTrivia annotation $
  maybe mempty (\k -> prettyExpr k <+> "=> ") mKey <>
  (if isUnpack then "..." else "") <>
  prettyExpr val

prettyMatchArm :: HasLeadingTrivia a => MatchArm a -> Doc ann
prettyMatchArm arm = prettyLeadingTrivia (matchArmAnnotation arm) $ case arm of
  MatchArm _ conds res ->
    hsep (punctuate "," (map prettyExpr conds)) <+> "=> " <> prettyExpr res
  MatchDefault _ res -> "default => " <> prettyExpr res

matchArmAnnotation :: MatchArm a -> a
matchArmAnnotation = \case
  MatchArm annotation _ _ -> annotation
  MatchDefault annotation _ -> annotation

prettyLiteral :: HasLeadingTrivia a => Literal a -> Doc ann
prettyLiteral literal = prettyLeadingTrivia (literalAnnotation literal) $ case literal of
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
      StrLit t -> pretty (escapeInterpText t)
      StrExpr e -> "{" <> prettyExpr e <> "}"

-- | Re-emit the escapes the lexer decoded away in interpolated-string text
-- parts: a backslash is doubled so it survives re-decoding, and a dollar that
-- would otherwise reparse as variable interpolation is escaped (Issue #88).
escapeInterpText :: Text -> Text
escapeInterpText = T.concat . go
  where
    go input = case T.uncons input of
      Nothing -> []
      Just ('\\', rest) -> "\\\\" : go rest
      Just ('$', rest) -> (if startsIdent rest then "\\$" else "$") : go rest
      Just (c, rest) -> T.singleton c : go rest
    startsIdent rest = case T.uncons rest of
      Just (c, _) -> isAlpha c || c == '_' || c >= '\x80'
      Nothing -> False

prettyVar :: HasLeadingTrivia a => Var a -> Doc ann
prettyVar variable = prettyLeadingTrivia (varAnnotation variable) $ case variable of
  SimpleVar _ vn -> prettyVarName vn
  DynamicVar _ (ExprVar _ innerVar) -> "$" <> prettyVar innerVar
  DynamicVar _ e -> "${" <> prettyExpr e <> "}"

literalAnnotation :: Literal a -> a
literalAnnotation = \case
  LitInt annotation _ _ -> annotation
  LitFloat annotation _ _ -> annotation
  LitString annotation _ _ -> annotation
  LitInterpolated annotation _ -> annotation
  LitHeredoc annotation _ _ _ -> annotation
  LitBool annotation _ -> annotation
  LitNull annotation -> annotation

varAnnotation :: Var a -> a
varAnnotation = \case
  SimpleVar annotation _ -> annotation
  DynamicVar annotation _ -> annotation

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

prettyIdent :: HasLeadingTrivia a => Ident a -> Doc ann
prettyIdent (Ident annotation name) = prettyLeadingTrivia annotation (pretty name)

prettyVarName :: HasLeadingTrivia a => VarName a -> Doc ann
prettyVarName (VarName annotation name) = prettyLeadingTrivia annotation ("$" <> pretty name)

prettyQualifiedName :: HasLeadingTrivia a => QualifiedName a -> Doc ann
prettyQualifiedName (QualifiedName annotation kind parts) = prettyLeadingTrivia annotation $
  case kind of
    NameFullyQualified -> "\\" <> pretty (T.intercalate "\\" parts)
    NameRelative -> "namespace\\" <> pretty (T.intercalate "\\" parts)
    _ -> pretty (T.intercalate "\\" parts)

-- | Pretty document for PHP Types (supports DNF, unions, intersections).
prettyType :: HasLeadingTrivia a => Type a -> Doc ann
prettyType typ = prettyLeadingTrivia (typeAnnotation typ) $ case typ of
  SimpleType _ qn -> prettyQualifiedName qn
  NullableType _ t -> "?" <> prettyType t
  UnionType _ ts -> hcat (punctuate "|" (map prettyType ts))
  IntersectionType _ ts -> hcat (punctuate "&" (map prettyType ts))
  DNFType _ ts -> parens (hcat (punctuate "&" (map prettyType ts)))

typeAnnotation :: Type a -> a
typeAnnotation = \case
  SimpleType a _ -> a
  NullableType a _ -> a
  UnionType a _ -> a
  IntersectionType a _ -> a
  DNFType a _ -> a
