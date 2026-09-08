{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Parser.Expression
  ( parseExpr
  , parseExprWith
  , parsePrimaryExpr
  , parseArg
  , parseCallArgs
  , parseMatchArm
  , parseArrayItem
  , parseAttributes
  , parseAttributeGroup
  , parseAttribute
  , exprSpan
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad (void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import Language.PHP.AST
import Language.PHP.Span (Span, combineSpans)
import Language.PHP.Parser.Lexer
import Language.PHP.Parser.Type (parseType, parseReturnType)

-- | Parse expression with default statement and class member dummies.
parseExpr :: Parser (Expr Span)
parseExpr = parseExprWith parseStmtDummy M.empty

-- | Parse expression parameterized by statement and class member parsers.
parseExprWith :: Parser (Stmt Span) -> Parser (ClassMember Span) -> Parser (Expr Span)
parseExprWith pStmt pMember = parseExprRec
  where
    parseExprRec = parseLogicalOr

    parseLogicalOr = parseBinaryLeft parseLogicalXor [ (keyword "or", OpLogicalOr) ]
    parseLogicalXor = parseBinaryLeft parseLogicalAnd [ (keyword "xor", OpLogicalXor) ]
    parseLogicalAnd = parseBinaryLeft parseAssignment [ (keyword "and", OpLogicalAnd) ]

    parseAssignment = parseYield <|> parseThrow <|> do
      lhs <- parseTernary
      assignRest lhs <|> pure lhs
      where
        assignRest lhs = do
          op <- parseAssignOp
          rhs <- parseAssignment
          let sp = combineSpans (exprSpan lhs) (exprSpan rhs)
          pure (ExprAssign sp op lhs rhs)

        parseAssignOp =
          (Nothing <$ lexeme (M.try (C.char '=' <* M.notFollowedBy (C.char '=' <|> C.char '>'))))
          <|> (Just OpAdd <$ symbol "+=")
          <|> (Just OpSub <$ symbol "-=")
          <|> (Just OpMul <$ symbol "*=")
          <|> (Just OpPow <$ symbol "**=")
          <|> (Just OpDiv <$ symbol "/=")
          <|> (Just OpMod <$ symbol "%=")
          <|> (Just OpConcat <$ symbol ".=")
          <|> (Just OpBitAnd <$ symbol "&=")
          <|> (Just OpBitOr <$ symbol "|=")
          <|> (Just OpBitXor <$ symbol "^=")
          <|> (Just OpShiftLeft <$ symbol "<<=")
          <|> (Just OpShiftRight <$ symbol ">>=")
          <|> (Just OpCoalesce <$ symbol "??=")

    parseYield = withSpan $ do
      _ <- keyword "yield"
      isFrom <- (True <$ keyword "from") <|> pure False
      if isFrom
        then do
          expr <- parseAssignment
          pure (\sp -> ExprYieldFrom sp expr)
        else do
          mKeyOrVal <- optional parseAssignment
          case mKeyOrVal of
            Nothing -> pure (\sp -> ExprYield sp Nothing Nothing)
            Just kOrV -> do
              isArrow <- (True <$ symbol "=>") <|> pure False
              if isArrow
                then do
                  val <- parseAssignment
                  pure (\sp -> ExprYield sp (Just kOrV) (Just val))
                else pure (\sp -> ExprYield sp Nothing (Just kOrV))

    parseThrow = withSpan $ do
      _ <- keyword "throw"
      expr <- parseAssignment
      pure (\sp -> ExprThrow sp expr)

    parseTernary = do
      cond <- parseCoalesce
      parseTernaryRest cond <|> pure cond
      where
        parseTernaryRest cond = do
          _ <- lexeme (M.try (C.char '?' <* M.notFollowedBy (C.char '?' <|> C.char '>')))
          isShort <- (True <$ symbol ":") <|> pure False
          if isShort
            then do
              fBranch <- parseAssignment
              let sp = combineSpans (exprSpan cond) (exprSpan fBranch)
              pure (ExprTernary sp cond Nothing fBranch)
            else do
              tBranch <- parseExprRec
              _ <- symbol ":"
              fBranch <- parseAssignment
              let sp = combineSpans (exprSpan cond) (exprSpan fBranch)
              pure (ExprTernary sp cond (Just tBranch) fBranch)

    parseCoalesce = do
      lhs <- parseBoolOr
      parseCoalesceRest lhs <|> pure lhs
      where
        parseCoalesceRest lhs = do
          _ <- lexeme (M.try (C.string "??" <* M.notFollowedBy (C.char '=')))
          rhs <- parseCoalesce <|> parseThrow
          let sp = combineSpans (exprSpan lhs) (exprSpan rhs)
          pure (ExprNullCoalesce sp lhs rhs)

    parseBoolOr = parseBinaryLeft parseBoolAnd [ (void (symbol "||"), OpBoolOr) ]
    parseBoolAnd = parseBinaryLeft parseBitOr [ (void (symbol "&&"), OpBoolAnd) ]
    parseBitOr = parseBinaryLeft parseBitXor [ (void (lexeme (M.try (C.char '|' <* M.notFollowedBy (C.char '|' <|> C.char '>' <|> C.char '=')))), OpBitOr) ]
    parseBitXor = parseBinaryLeft parseBitAnd [ (void (lexeme (M.try (C.char '^' <* M.notFollowedBy (C.char '=')))), OpBitXor) ]
    parseBitAnd = parseBinaryLeft parseEquality [ (void (lexeme (M.try (C.char '&' <* M.notFollowedBy (C.char '&' <|> C.char '=')))), OpBitAnd) ]

    parseEquality = parseBinaryLeft parseComparison
      [ (void (symbol "==="), OpIdentical)
      , (void (symbol "!=="), OpNotIdentical)
      , (void (symbol "=="), OpEq)
      , (void (symbol "!="), OpNotEq)
      , (void (symbol "<>"), OpNotEq)
      ]

    parseComparison = parseBinaryLeft parsePipe
      [ (void (symbol "<=>"), OpSpaceship)
      , (void (symbol "<="), OpLte)
      , (void (symbol ">="), OpGte)
      , (void (lexeme (M.try (C.char '<' <* M.notFollowedBy (C.char '<' <|> C.char '=' <|> C.char '>')))), OpLt)
      , (void (lexeme (M.try (C.char '>' <* M.notFollowedBy (C.char '>' <|> C.char '=')))), OpGt)
      , (void (keyword "instanceof"), OpInstanceof)
      ]

    parsePipe = parseBinaryLeft parseShift [ (void (symbol "|>"), OpPipe) ]

    parseShift = parseBinaryLeft parseAddSub
      [ (void (symbol "<<"), OpShiftLeft)
      , (void (symbol ">>"), OpShiftRight)
      ]

    parseAddSub = parseBinaryLeft parseMulDivMod
      [ (void (lexeme (M.try (C.char '+' <* M.notFollowedBy (C.char '+' <|> C.char '=')))), OpAdd)
      , (void (lexeme (M.try (C.char '-' <* M.notFollowedBy (C.char '-' <|> C.char '>' <|> C.char '=')))), OpSub)
      , (void (lexeme (M.try (C.char '.' <* M.notFollowedBy (C.char '.' <|> C.char '=')))), OpConcat)
      ]

    parseMulDivMod = parseBinaryLeft parseExponentiation
      [ (void (lexeme (M.try (C.char '*' <* M.notFollowedBy (C.char '*' <|> C.char '=')))), OpMul)
      , (void (lexeme (M.try (C.char '/' <* M.notFollowedBy (C.char '/' <|> C.char '*' <|> C.char '=')))), OpDiv)
      , (void (lexeme (M.try (C.char '%' <* M.notFollowedBy (C.char '=')))), OpMod)
      ]

    parseExponentiation = do
      lhs <- parseUnary
      (do
        _ <- symbol "**"
        rhs <- parseExponentiation
        let sp = combineSpans (exprSpan lhs) (exprSpan rhs)
        pure (ExprBinary sp OpPow lhs rhs)) <|> pure lhs

    parseUnary = parseClone <|> parsePrefix <|> parseCast <|> parsePostfix
      where
        parsePrefix = withSpan $ do
          op <- (OpPreInc <$ symbol "++")
                <|> (OpPreDec <$ symbol "--")
                <|> (OpBoolNot <$ symbol "!")
                <|> (OpBitNot <$ symbol "~")
                <|> (OpUnaryPlus <$ symbol "+")
                <|> (OpUnaryMinus <$ symbol "-")
                <|> (OpErrorSuppress <$ symbol "@")
          operand <- parseUnary
          pure (\sp -> ExprUnary sp op operand)

        parseCast = withSpan $ M.try $ do
          _ <- symbol "("
          castType <- parseCastType
          _ <- symbol ")"
          operand <- parseUnary
          pure (\sp -> ExprCast sp castType operand)

        parseCastType =
          (CastInt <$ (keyword "int" <|> keyword "integer"))
          <|> (CastFloat <$ (keyword "float" <|> keyword "double" <|> keyword "real"))
          <|> (CastString <$ (keyword "string" <|> keyword "binary"))
          <|> (CastBool <$ (keyword "bool" <|> keyword "boolean"))
          <|> (CastArray <$ keyword "array")
          <|> (CastObject <$ keyword "object")
          <|> (CastUnset <$ keyword "unset")

    parseClone = withSpan $ do
      _ <- keyword "clone"
      isParen <- (True <$ M.lookAhead (symbol "(")) <|> pure False
      if isParen
        then parens $ do
          obj <- parseExprRec
          mWith <- optional (comma *> parseCloneWithPayload)
          pure (\sp -> ExprClone sp obj mWith)
        else do
          obj <- parseUnary
          pure (\sp -> ExprClone sp obj Nothing)
      where
        parseCloneWithPayload = do
          _ <- optional (keyword "with" *> colon)
          items <- brackets (parseClonePair `M.sepEndBy` comma)
          pure items

        parseClonePair = do
          k <- parseExprRec
          _ <- symbol "=>"
          v <- parseExprRec
          pure (k, v)

    parsePostfix = do
      base <- parsePrimary
      chainPostfix base

    chainPostfix base = do
      mNext <- optional (parseOnePostfix base)
      case mNext of
        Nothing -> pure base
        Just next -> chainPostfix next

    parseOnePostfix base =
      parsePostInc
      <|> parsePostDec
      <|> parseMethodOrProp
      <|> parseNullsafeMethodOrProp
      <|> parseStaticAccess
      <|> parseArrayAccess
      <|> parseCall
      where
        parsePostInc = do
          (spEnd, _) <- spanned (symbol "++")
          pure (ExprUnary (combineSpans (exprSpan base) spEnd) OpPostInc base)

        parsePostDec = do
          (spEnd, _) <- spanned (symbol "--")
          pure (ExprUnary (combineSpans (exprSpan base) spEnd) OpPostDec base)

        parseMethodOrProp = do
          _ <- symbol "->"
          name <- parseMemberName
          mArgs <- optional parseCallArgs
          let sp = combineSpans (exprSpan base) (memberNameSpan name)
          case mArgs of
            Nothing -> pure (ExprPropertyFetch sp base name)
            Just args -> pure (ExprMethodCall sp base name args)

        parseNullsafeMethodOrProp = do
          _ <- symbol "?->"
          name <- parseMemberName
          mArgs <- optional parseCallArgs
          let sp = combineSpans (exprSpan base) (memberNameSpan name)
          case mArgs of
            Nothing -> pure (ExprNullsafePropertyFetch sp base name)
            Just args -> pure (ExprNullsafeMethodCall sp base name args)

        parseStaticAccess = do
          _ <- doubleColon
          target <- parseStaticTarget
          let classTarget = toClassTarget base
          case target of
            Left varName -> do
              let sp = combineSpans (exprSpan base) (varNameSpan varName)
              pure (ExprStaticPropertyFetch sp classTarget varName)
            Right constOrMethod -> do
              mArgs <- optional parseCallArgs
              let sp = combineSpans (exprSpan base) (classConstSpan constOrMethod)
              case mArgs of
                Nothing -> pure (ExprClassConstFetch sp classTarget constOrMethod)
                Just args ->
                  let member = case constOrMethod of
                        ConstNameIdent id' -> MemberIdent id'
                        ConstNameDynamic e -> MemberExpr e
                  in pure (ExprStaticCall sp classTarget member args)

        parseArrayAccess = do
          _ <- symbol "["
          mIdx <- optional parseExprRec
          (spEnd, _) <- spanned (symbol "]")
          let sp = combineSpans (exprSpan base) spEnd
          pure (ExprArrayAccess sp base mIdx)

        parseCall = do
          (spEnd, args) <- spanned parseCallArgs
          let sp = combineSpans (exprSpan base) spEnd
          pure (ExprCall sp base args)

    parseStaticTarget =
      (Left <$> variableName)
      <|> (Right <$> parseClassConstName)

    parseClassConstName =
      dynamicConst
      <|> (ConstNameIdent <$> parseAnyIdent)
      where
        dynamicConst = braces $ do
          expr <- parseExprRec
          pure (ConstNameDynamic expr)

        parseAnyIdent = withSpan $ do
          tok <- rawIdentifier
          _ <- sc
          pure (\sp -> Ident sp tok)

    parseMemberName =
      (MemberExpr <$> braces parseExprRec)
      <|> (MemberExpr <$> parseVariableExpr)
      <|> (MemberIdent <$> parseAnyIdent)
      where
        parseAnyIdent = withSpan $ do
          tok <- rawIdentifier
          _ <- sc
          pure (\sp -> Ident sp tok)

    parsePrimary =
      parseNew
      <|> parseMatch
      <|> parseArrowFunction
      <|> parseClosure
      <|> parseArrayLit
      <|> parseVariableExpr
      <|> parseLiteralExpr
      <|> parseConstFetch
      <|> parens parseExprRec

    parseConstFetch = withSpan $ do
      qn <- qualifiedName
      pure (\sp -> ExprConstFetch sp qn)

    parseNew = withSpan $ M.try $ do
      keyword_ "new"
      isReadonlyAnon <- (True <$ M.try (keyword "readonly" *> keyword "class")) <|> pure False
      isAnon <- if isReadonlyAnon then pure True else (True <$ keyword "class") <|> pure False
      if isAnon
        then do
          let modif = ClassModifier False False isReadonlyAnon
          mArgs <- optional (parens (parseArgWith parseExprRec `M.sepEndBy` comma))
          let args = maybe [] id mArgs
          mExtends <- optional (keyword "extends" *> qualifiedName)
          impls <- (keyword "implements" *> (qualifiedName `M.sepBy1` comma)) <|> pure []
          members <- braces (M.many pMember)
          pure (\sp -> ExprNewAnonClass sp [] modif args mExtends impls members)
        else do
          target <- parseNewTarget
          mArgs <- optional (parens (parseArgWith parseExprRec `M.sepEndBy` comma))
          let args = maybe [] id mArgs
          pure (\sp -> ExprNew sp target args)
      where
        parseNewTarget =
          (ClassTargetExpr <$> parens parseExprRec)
          <|> (parseVariableExpr >>= chainDynamicTarget)
          <|> M.try (do
                QualifiedName qnSp kind parts <- qualifiedName
                let qn = QualifiedName qnSp kind parts
                _ <- doubleColon
                vn <- variableName
                let sp = combineSpans qnSp (varNameSpan vn)
                chainDynamicTarget (ExprStaticPropertyFetch sp (ClassTargetName qn) vn))
          <|> (ClassTargetName <$> qualifiedName)

        chainDynamicTarget base = do
          mNext <- optional (parseDynamicStep base)
          case mNext of
            Nothing   -> pure (toClassTarget base)
            Just next -> chainDynamicTarget next

        parseDynamicStep base =
          parseProp
          <|> parseNullsafeProp
          <|> parseStaticProp
          <|> parseArr
          where
            parseProp = do
              _ <- symbol "->"
              name <- parseMemberName
              let sp = combineSpans (exprSpan base) (memberNameSpan name)
              pure (ExprPropertyFetch sp base name)

            parseNullsafeProp = do
              _ <- symbol "?->"
              name <- parseMemberName
              let sp = combineSpans (exprSpan base) (memberNameSpan name)
              pure (ExprNullsafePropertyFetch sp base name)

            parseStaticProp = do
              _ <- doubleColon
              vn <- variableName
              let sp = combineSpans (exprSpan base) (varNameSpan vn)
              pure (ExprStaticPropertyFetch sp (toClassTarget base) vn)

            parseArr = do
              _ <- symbol "["
              mIdx <- optional parseExprRec
              (spEnd, _) <- spanned (symbol "]")
              let sp = combineSpans (exprSpan base) spEnd
              pure (ExprArrayAccess sp base mIdx)

    parseMatch = withSpan $ do
      _ <- keyword "match"
      subject <- parens parseExprRec
      arms <- braces (M.try (parseMatchArmWith parseExprRec) `M.sepEndBy` comma)
      pure (\sp -> ExprMatch sp subject arms)

    parseArrowFunction = withSpan $ M.try $ do
      attrs1 <- parseAttributes
      isStatic <- (True <$ keyword "static") <|> pure False
      attrs2 <- if isStatic then parseAttributes else pure []
      let attrs = attrs1 ++ attrs2
      keyword_ "fn"
      byRef <- (True <$ symbol "&") <|> pure False
      params <- parens (parseParamDummy parseExprRec `M.sepEndBy` comma)
      retType <- parseReturnType
      _ <- symbol "=>"
      body <- parseAssignment
      pure (\sp -> ExprArrowFunction sp attrs byRef isStatic params retType body)

    parseClosure = withSpan $ M.try $ do
      attrs1 <- parseAttributes
      isStatic <- (True <$ keyword "static") <|> pure False
      attrs2 <- if isStatic then parseAttributes else pure []
      let attrs = attrs1 ++ attrs2
      keyword_ "function"
      byRef <- (True <$ symbol "&") <|> pure False
      params <- parens (parseParamDummy parseExprRec `M.sepEndBy` comma)
      uses <- (keyword "use" *> parens (parseClosureUse `M.sepEndBy` comma)) <|> pure []
      retType <- parseReturnType
      body <- braces (M.many pStmt)
      pure (\sp -> ExprClosure sp attrs byRef isStatic params uses retType body)
      where
        parseClosureUse = do
          isRef <- (True <$ symbol "&") <|> pure False
          var <- variableName
          pure (var, isRef)

    parseArrayLit = withSpan $ (brackets parseItems <|> (keyword "array" *> parens parseItems))
      where
        parseItems = do
          items <- parseArrayItemWith parseExprRec `M.sepEndBy` comma
          pure (\sp -> ExprArray sp items)

    parseVariableExpr = withSpan $ do
      v <- parseVar
      pure (\sp -> ExprVar sp v)
      where
        parseVar = parseSimple <|> parseDynamic
        parseSimple = withSpan $ M.try $ do
          vn <- variableName
          pure (\sp -> SimpleVar sp vn)
        parseDynamic = withSpan $ do
          _ <- symbol "$"
          inner <- parseBraced <|> parseVariableExpr
          pure (\sp -> DynamicVar sp inner)
        parseBraced = braces parseExprRec

    parseLiteralExpr = withSpan $ do
      lit <- parseLit
      pure (\sp -> ExprLit sp lit)
      where
        parseLit =
          literalFloat
          <|> literalInt
          <|> literalString
          <|> literalHeredocOrNowdoc
          <|> parseBool
          <|> parseNull

        parseBool = withSpan $ do
          val <- (True <$ keyword "true") <|> (False <$ keyword "false")
          pure (\sp -> LitBool sp val)

        parseNull = withSpan $ do
          keyword_ "null"
          pure (\sp -> LitNull sp)

    parseBinaryLeft next ops = do
      lhs <- next
      parseRest lhs
      where
        parseRest lhs = do
          mOp <- optional (M.choice [op <$ p | (p, op) <- ops])
          case mOp of
            Nothing -> pure lhs
            Just op -> do
              rhs <- next
              let sp = combineSpans (exprSpan lhs) (exprSpan rhs)
              parseRest (ExprBinary sp op lhs rhs)

-- | Primary expression parser exposed to public / tests.
parsePrimaryExpr :: Parser (Expr Span)
parsePrimaryExpr = parseExpr

-- | Single call argument using expression parser.
parseArg :: Parser (Arg Span)
parseArg = parseArgWith parseExpr

parseArgWith :: Parser (Expr Span) -> Parser (Arg Span)
parseArgWith pExpr = withSpan $ do
  mName <- optional (M.try (spanned (rawIdentifier <* sc) <* colon))
  let mIdent = case mName of
        Nothing -> Nothing
        Just (spId, n) -> Just (Ident spId n)
  isUnpack <- (True <$ symbol "...") <|> pure False
  expr <- pExpr
  pure (\sp -> Arg sp mIdent expr isUnpack)

-- | Call arguments list: (arg1, arg2) or first-class callable (...)
parseCallArgs :: Parser (CallArgs Span)
parseCallArgs = parens $ do
  isCallable <- (True <$ symbol "...") <|> pure False
  if isCallable
    then pure FirstClassCallable
    else ArgsList <$> (parseArg `M.sepEndBy` comma)

-- | Match arm using expression parser.
parseMatchArm :: Parser (MatchArm Span)
parseMatchArm = parseMatchArmWith parseExpr

parseMatchArmWith :: Parser (Expr Span) -> Parser (MatchArm Span)
parseMatchArmWith pExpr = withSpan $ do
  isDefault <- (True <$ keyword "default") <|> pure False
  if isDefault
    then do
      _ <- symbol "=>"
      body <- pExpr
      pure (\sp -> MatchDefault sp body)
    else do
      conds <- pExpr `M.sepBy1` comma
      _ <- symbol "=>"
      body <- pExpr
      pure (\sp -> MatchArm sp conds body)

-- | Array item using expression parser.
parseArrayItem :: Parser (ArrayItem Span)
parseArrayItem = parseArrayItemWith parseExpr

parseArrayItemWith :: Parser (Expr Span) -> Parser (ArrayItem Span)
parseArrayItemWith pExpr = withSpan $ do
  isSpread <- (True <$ symbol "...") <|> pure False
  if isSpread
    then do
      expr <- pExpr
      pure (\sp -> ArrayItem sp Nothing expr True)
    else do
      kOrV <- pExpr
      isArrow <- (True <$ symbol "=>") <|> pure False
      if isArrow
        then do
          v <- pExpr
          pure (\sp -> ArrayItem sp (Just kOrV) v False)
        else pure (\sp -> ArrayItem sp Nothing kOrV False)

-- | Attributes #[ ... ]
parseAttributes :: Parser [AttributeGroup Span]
parseAttributes = M.many parseAttributeGroup

-- | Parse a single attribute group @#[ ... ]@.
parseAttributeGroup :: Parser (AttributeGroup Span)
parseAttributeGroup = withSpan $ do
  _ <- symbol "#["
  attrs <- parseAttribute `M.sepEndBy1` comma
  _ <- symbol "]"
  pure (\sp -> AttributeGroup sp attrs)

-- | Parse an attribute declaration within an attribute group.
parseAttribute :: Parser (Attribute Span)
parseAttribute = withSpan $ do
  qn <- qualifiedName
  mArgs <- optional (parens (parseArg `M.sepEndBy` comma))
  pure (\sp -> Attribute sp qn (maybe [] id mArgs))

parseParamDummy :: Parser (Expr Span) -> Parser (Param Span)
parseParamDummy pExpr = withSpan $ do
  attrs <- parseAttributes
  typ <- optional parseType
  byRef <- (True <$ symbol "&") <|> pure False
  isVariadic <- (True <$ symbol "...") <|> pure False
  var <- variableName
  mDef <- optional (symbol "=" *> pExpr)
  pure (\sp -> Param sp attrs Nothing Nothing False typ byRef isVariadic var mDef)

parseStmtDummy :: Parser (Stmt Span)
parseStmtDummy = withSpan $ do
  expr <- parseExpr
  _ <- semi
  pure (\sp -> StmtExpr sp expr)

-- | Helper to extract span of any expression.
exprSpan :: Expr Span -> Span
exprSpan = getAnnotation

varNameSpan :: VarName Span -> Span
varNameSpan (VarName sp _) = sp

memberNameSpan :: MemberName Span -> Span
memberNameSpan = \case
  MemberIdent (Ident sp _) -> sp
  MemberExpr e             -> exprSpan e

classConstSpan :: ClassConstName Span -> Span
classConstSpan = \case
  ConstNameIdent (Ident sp _) -> sp
  ConstNameDynamic e          -> exprSpan e

-- | Convert an expression to a class target (e.g. constant fetch -> ClassTargetName).
toClassTarget :: Expr a -> ClassTarget a
toClassTarget (ExprConstFetch _ qn) = ClassTargetName qn
toClassTarget e                     = ClassTargetExpr e
