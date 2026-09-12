{-# LANGUAGE OverloadedStrings #-}

module Language.PHP.Parser.Expression
  ( parseExpr
  , parseExprWith
  , parseExprWithContext
  , parsePrimaryExpr
  , parseArg
  , parseCallArgs
  , parseMatchArm
  , parseArrayItem
  , parseAttributes
  , parseAttributeGroup
  , parseAttribute
  , parseLiteral
  , parseLiteralWith
  , exprSpan
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad (guard, join, void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import Language.PHP.AST
import Language.PHP.Span (Span, combineSpans)
import Language.PHP.Parser.Lexer
import Language.PHP.Parser.Type (parseType, parseReturnType)

-- | Whether an expression denotes a PHP @variable@, the only kind of source a
-- by-reference assignment can bind to.
isReferenceable :: Expr a -> Bool
isReferenceable = \case
  ExprVar {}                   -> True
  ExprArrayAccess {}           -> True
  ExprPropertyFetch {}         -> True
  ExprNullsafePropertyFetch {} -> True
  ExprStaticPropertyFetch {}   -> True
  ExprCall {}                  -> True
  ExprMethodCall {}            -> True
  ExprNullsafeMethodCall {}    -> True
  ExprStaticCall {}            -> True
  _                            -> False

-- | Parse expression with default statement and class member dummies.
parseExpr :: Parser (Expr Span)
parseExpr = parseExprWith parseStmtDummy M.empty

-- | Parse expression parameterized by statement and class member parsers.
parseExprWith :: Parser (Stmt Span) -> Parser (ClassMember Span) -> Parser (Expr Span)
parseExprWith pStmt pMember = parseExprWithContext pStmt (const pMember)

-- | Parse an expression with a class member parser that receives the
-- enclosing class's readonly status for anonymous classes.
parseExprWithContext :: Parser (Stmt Span) -> (Bool -> Parser (ClassMember Span)) -> Parser (Expr Span)
parseExprWithContext pStmt pMember = parseExprRec
  where
    parseExprRec = parseLogicalOr

    parseLogicalOr = parseBinaryLeft parseLogicalXor [ (keyword "or", OpLogicalOr) ]
    parseLogicalXor = parseBinaryLeft parseLogicalAnd [ (keyword "xor", OpLogicalXor) ]
    parseLogicalAnd = parseBinaryLeft parseAssignment [ (keyword "and", OpLogicalAnd) ]

    parseAssignment = parseYield <|> parseThrow <|> parseInclude <|> parsePrint <|> do
      lhs <- parseTernary
      assignRest lhs <|> pure lhs
      where
        assignRest lhs = do
          op <- parseAssignOp
          case op of
            Nothing -> assignRefRest lhs <|> assignValueRest Nothing lhs
            Just _  -> assignValueRest op lhs

        -- By-reference assignment. The ampersand belongs to the operator, not
        -- to the source expression, so @$a =& $b@ and @$a = &$b@ differ only in
        -- trivia and share this production (Issue #138).
        assignRefRest lhs = do
          _ <- symbol "&"
          rhs <- parseReferenceSource
          let sp = combineSpans (exprSpan lhs) (exprSpan rhs)
          pure (ExprAssignRef sp lhs rhs)

        assignValueRest op lhs = do
          rhs <- parseAssignment
          let sp = combineSpans (exprSpan lhs) (exprSpan rhs)
          pure (ExprAssign sp op lhs rhs)

        -- PHP takes a reference to a variable, never to a value: the source of
        -- a by-reference assignment is drawn from the postfix level and must be
        -- variable-like, so @$a =& new Foo()@ and @$a =& 1@ are syntax errors.
        parseReferenceSource = do
          rhs <- parsePostfix
          guard (isReferenceable rhs)
          pure rhs

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

    parseInclude = withSpan $ do
      incType <- parseIncludeType
      expr <- parseAssignment
      pure (\sp -> ExprInclude sp incType expr)

    parsePrint = withSpan $ do
      _ <- keyword "print"
      expr <- parseAssignment
      pure (\sp -> ExprPrint sp expr)

    parseIncludeType =
      (IncIncludeOnce <$ keyword "include_once")
      <|> (IncInclude <$ keyword "include")
      <|> (IncRequireOnce <$ keyword "require_once")
      <|> (IncRequire <$ keyword "require")

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

    -- @instanceof@ binds below unary operators and exponentiation, but above
    -- multiplicative, additive, shift, pipe, and comparison operators.
    parseInstanceof = parseBinaryLeft parseExponentiation
      [ (void (keyword "instanceof"), OpInstanceof)
      ]

    parseMulDivMod = parseBinaryLeft parseInstanceof
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

    parseUnary = parseClone <|> parseIncDec <|> parsePrefix <|> parseCast <|> parsePostfix
      where
        -- Prefix ++/-- take a variable, so they bind tighter than "**".
        parseIncDec = withSpan $ do
          op <- (OpPreInc <$ symbol "++") <|> (OpPreDec <$ symbol "--")
          operand <- parseUnary
          pure (\sp -> ExprUnary sp op operand)

        -- The remaining prefix operators bind looser than "**", so "-2 ** 2"
        -- is "-(2 ** 2)"; their operand is parsed at exponentiation level.
        parsePrefix = withSpan $ do
          op <- (OpBoolNot <$ symbol "!")
                <|> (OpBitNot <$ symbol "~")
                <|> (OpUnaryPlus <$ symbol "+")
                <|> (OpUnaryMinus <$ symbol "-")
                <|> (OpErrorSuppress <$ symbol "@")
          operand <- parseExponentiation
          pure (\sp -> ExprUnary sp op operand)

        -- Casts likewise bind looser than "**".
        parseCast = withSpan $ M.try $ do
          _ <- symbol "("
          castType <- parseCastType
          _ <- symbol ")"
          operand <- parseExponentiation
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
      cloneWith <|> cloneOperand
      where
        cloneWith = M.try $ parens $ do
          obj <- parseExprRec
          _ <- comma
          mPayload <- optional parseCloneWithPayload
          _ <- optional comma
          pure (\sp -> ExprClone sp obj mPayload)

        cloneOperand = do
          obj <- parseUnary
          pure (\sp -> ExprClone sp obj Nothing)

        parseCloneWithPayload = do
          _ <- optional (M.try (keyword "with" *> colon))
          parseExprRec

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
              mArgs <- optional parseCallArgs
              case mArgs of
                Nothing -> pure (ExprStaticPropertyFetch sp classTarget varName)
                Just args ->
                  let varExpr = ExprVar (varNameSpan varName)
                                  (SimpleVar (varNameSpan varName) varName)
                  in pure (ExprStaticCall sp classTarget (MemberExpr varExpr) args)
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
      <|> parseListLit
      <|> parseVariableExpr
      <|> parseLiteralExpr
      <|> parseIsset
      <|> parseEmpty
      <|> parseEval
      <|> parseExit
      <|> parseConstFetch
      <|> parens parseExprRec

    parseIsset = withSpan $ do
      keyword_ "isset"
      args <- parens (parseExprRec `M.sepEndBy1` comma)
      pure (\sp -> ExprIsset sp args)

    parseEmpty = withSpan $ do
      keyword_ "empty"
      expr <- parens parseExprRec
      pure (\sp -> ExprEmpty sp expr)

    parseEval = withSpan $ do
      keyword_ "eval"
      expr <- parens parseExprRec
      pure (\sp -> ExprEval sp expr)

    -- @exit@ and @die@ take an optional parenthesized status, so @exit@ and
    -- @exit()@ are the same statusless construct.
    parseExit = withSpan $ do
      kind <- (ExitExit <$ keyword "exit") <|> (ExitDie <$ keyword "die")
      mStatus <- optional (parens (optional parseExprRec))
      pure (\sp -> ExprExit sp kind (join mStatus))

    parseConstFetch = withSpan $ do
      qn <- parseClassName
      pure (\sp -> ExprConstFetch sp qn)

    parseNew = withSpan $ M.try $ do
      keyword_ "new"
      attrs <- parseAttributes
      isReadonlyAnon <- (True <$ M.try (keyword "readonly" *> keyword "class")) <|> pure False
      isAnon <- if isReadonlyAnon then pure True else (True <$ keyword "class") <|> pure False
      if isAnon
        then do
          let modif = ClassModifier False False isReadonlyAnon
          mArgs <- optional (parens (parseArgWith parseExprRec `M.sepEndBy` comma))
          let args = maybe [] id mArgs
          mExtends <- optional (keyword "extends" *> qualifiedName)
          impls <- (keyword "implements" *> (qualifiedName `M.sepBy1` comma)) <|> pure []
          members <- braces (M.many (pMember isReadonlyAnon))
          pure (\sp -> ExprNewAnonClass sp attrs modif args mExtends impls members)
        else do
          guard (null attrs)
          target <- parseNewTarget
          mArgs <- optional (parens (parseArgWith parseExprRec `M.sepEndBy` comma))
          case mArgs of
            Nothing -> do
              -- An unparenthesized @new@ over a named class is not
              -- dereferenceable in PHP: @new Foo->bar@ is a syntax error,
              -- while @new Foo()->bar@ is valid (Issue #130). Anonymous
              -- classes @new class { ... }@ are terminated by their braces
              -- and may be dereferenced without call parentheses.
              _ <- M.notFollowedBy
                (M.lookAhead (symbol "->" <|> symbol "?->" <|> doubleColon <|> symbol "["))
              pure (\sp -> ExprNew sp target [])
            Just args -> pure (\sp -> ExprNew sp target args)
      where
        parseNewTarget =
          (ClassTargetExpr <$> parens parseExprRec)
          <|> (parseVariableExpr >>= chainDynamicTarget)
          <|> M.try (do
                QualifiedName qnSp kind parts <- parseClassName
                let qn = QualifiedName qnSp kind parts
                _ <- doubleColon
                vn <- variableName
                let sp = combineSpans qnSp (varNameSpan vn)
                chainDynamicTarget (ExprStaticPropertyFetch sp (ClassTargetName qn) vn))
          <|> (ClassTargetName <$> parseClassName)

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

    parseListLit = withSpan $ do
      keyword_ "list"
      items <- parens (parseArrayItemWith parseExprRec `M.sepEndBy` comma)
      pure (\sp -> ExprList sp items)

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
      lit <- parseLiteralWith parseExprRec
      pure (\sp -> ExprLit sp lit)

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

-- | Parse a literal using a custom expression parser for string interpolations.
parseLiteralWith :: Parser (Expr Span) -> Parser (Literal Span)
parseLiteralWith pExpr =
  literalFloat
  <|> literalInt
  <|> literalString pExpr
  <|> literalHeredocOrNowdoc
  <|> parseBool
  <|> parseNull
  where
    parseBool = withSpan $ do
      val <- (True <$ keyword "true") <|> (False <$ keyword "false")
      pure (\sp -> LitBool sp val)

    parseNull = withSpan $ do
      keyword_ "null"
      pure (\sp -> LitNull sp)

-- | Parse a literal using default expression parser.
parseLiteral :: Parser (Literal Span)
parseLiteral = parseLiteralWith parseExpr

-- | Single call argument using expression parser.
parseArg :: Parser (Arg Span)
parseArg = parseArgWith parseExpr

parseArgWith :: Parser (Expr Span) -> Parser (Arg Span)
parseArgWith pExpr = withSpan $ do
  mName <- optional (M.try (spanned (rawIdentifier <* sc) <* colon <* M.notFollowedBy (C.char ':')))
  let mIdent = case mName of
        Nothing -> Nothing
        Just (spId, n) -> Just (Ident spId n)
  isUnpack <- (True <$ symbol "...") <|> pure False
  expr <- pExpr
  pure (\sp -> Arg sp mIdent expr isUnpack)

-- | Call arguments list: (arg1, arg2) or first-class callable (...)
parseCallArgs :: Parser (CallArgs Span)
parseCallArgs = parens $
  (FirstClassCallable <$ M.try (symbol "..." <* M.lookAhead (symbol ")")))
  <|> (ArgsList <$> (parseArg `M.sepEndBy` comma))

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
      conds <- pExpr `M.sepEndBy1` comma
      _ <- symbol "=>"
      body <- pExpr
      pure (\sp -> MatchArm sp conds body)

-- | Array item using expression parser.
parseArrayItem :: Parser (ArrayItem Span)
parseArrayItem = parseArrayItemWith parseExpr

parseArrayItemWith :: Parser (Expr Span) -> Parser (ArrayItem Span)
parseArrayItemWith pExpr = parseOmittedSlot <|> parseItem
  where
    parseOmittedSlot = do
      _ <- M.lookAhead comma
      withSpan (pure (\sp -> ArrayItemEmpty sp))

    parseItem = withSpan $ do
      isSpread <- (True <$ symbol "...") <|> pure False
      if isSpread
        then do
          expr <- pExpr
          pure (\sp -> ArrayItem sp Nothing expr True False)
        else do
          isRef <- (True <$ symbol "&") <|> pure False
          if isRef
            then do
              expr <- pExpr
              pure (\sp -> ArrayItem sp Nothing expr False True)
            else do
              kOrV <- pExpr
              isArrow <- (True <$ symbol "=>") <|> pure False
              if isArrow
                then do
                  valRef <- (True <$ symbol "&") <|> pure False
                  v <- pExpr
                  pure (\sp -> ArrayItem sp (Just kOrV) v False valRef)
                else pure (\sp -> ArrayItem sp Nothing kOrV False False)

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

-- | Class name, including late static binding `static`.
parseClassName :: Parser (QualifiedName Span)
parseClassName = qualifiedName <|> staticClassName
  where
    staticClassName = withSpan $ do
      tok <- keyword "static"
      pure (\sp -> QualifiedName sp NameUnqualified [tok])
