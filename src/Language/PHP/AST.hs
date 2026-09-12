{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.PHP.AST
  ( Program (..)
  , Stmt (..)
  , Expr (..)
  , Literal (..)
  , StringPart (..)
  , Var (..)
  , VarName (..)
  , Ident (..)
  , QualifiedName (..)
  , NameKind (..)
  , Type (..)
  , Visibility (..)
  , PropertyModifier (..)
  , MethodModifier (..)
  , ClassModifier (..)
  , PropertyHook (..)
  , HookType (..)
  , HookBody (..)
  , PropertyDecl (..)
  , ConstDecl (..)
  , ClassMember (..)
  , Param (..)
  , MethodDecl (..)
  , FunctionDecl (..)
  , ClassDecl (..)
  , InterfaceDecl (..)
  , TraitDecl (..)
  , EnumDecl (..)
  , EnumCase (..)
  , TraitUse (..)
  , TraitAdaptation (..)
  , AttributeGroup (..)
  , Attribute (..)
  , Arg (..)
  , MatchArm (..)
  , CatchClause (..)
  , SwitchCase (..)
  , ArrayItem (..)
  , CallArgs (..)
  , ClassTarget (..)
  , ClassConstName (..)
  , MemberName (..)
  , BinOp (..)
  , UnOp (..)
  , CastType (..)
  , IncludeType (..)
  , ExitKind (..)
  , UseType (..)
  , UseClause (..)
  , DeclareDirective (..)
  , Trivia (..)
  , Annotated (..)
  , getAnnotation
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)

-- | Trivia such as comments and PHPDoc blocks.
data Trivia
  = CommentLine !Text
  | CommentBlock !Text
  | DocBlock !Text
  deriving (Eq, Ord, Show, Generic)

-- | Annotation wrapper that pairs an AST node's metadata with leading comments.
data Annotated a = Annotated
  { annValue   :: !a
  , annTrivia  :: ![Trivia]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | PHP Identifier (e.g., function name, class name, method name).
data Ident a = Ident !a !Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | PHP Variable Name without leading '$' (e.g. "foo" for "$foo").
data VarName a = VarName !a !Text
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Name qualification style.
data NameKind
  = NameUnqualified
  | NameQualified
  | NameFullyQualified
  | NameRelative
  deriving (Eq, Ord, Show, Generic)

-- | PHP Qualified or Unqualified Name (e.g. "App\\Models\\User", "\\Exception").
data QualifiedName a = QualifiedName !a !NameKind ![Text]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | PHP Types supporting PHP 8.0-8.5: simple, nullable, union, intersection, and DNF.
data Type a
  = SimpleType !a !(QualifiedName a)
  | NullableType !a !(Type a)
  | UnionType !a ![Type a]
  | IntersectionType !a ![Type a]
  | DNFType !a ![Type a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Visibility levels.
data Visibility
  = Public
  | Protected
  | Private
  deriving (Eq, Ord, Show, Generic)

-- | Property modifiers including asymmetric visibility (PHP 8.4/8.5).
data PropertyModifier = PropertyModifier
  { propVis      :: !(Maybe Visibility)
  , propWriteVis :: !(Maybe Visibility) -- ^ Asymmetric visibility, e.g. private(set)
  , propStatic   :: !Bool
  , propReadonly :: !Bool
  , propFinal    :: !Bool
  , propAbstract :: !Bool
  } deriving (Eq, Ord, Show, Generic)

-- | Method modifiers.
data MethodModifier = MethodModifier
  { methodVis      :: !(Maybe Visibility)
  , methodStatic   :: !Bool
  , methodFinal    :: !Bool
  , methodAbstract :: !Bool
  } deriving (Eq, Ord, Show, Generic)

-- | Class modifiers.
data ClassModifier = ClassModifier
  { classFinal    :: !Bool
  , classAbstract :: !Bool
  , classReadonly :: !Bool -- ^ PHP 8.2 readonly class
  } deriving (Eq, Ord, Show, Generic)

-- | Property hook type (PHP 8.4).
data HookType = HookGet | HookSet
  deriving (Eq, Ord, Show, Generic)

-- | Body of a property hook.
data HookBody a
  = HookExpr !(Expr a)
  | HookBlock ![Stmt a]
  | HookAbstract -- ^ Bodyless hook in interfaces/abstract classes (e.g. @get;@)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | PHP 8.4 Property Hook (e.g. get => $this->name; or set(string $value) { ... }).
data PropertyHook a = PropertyHook
  { hookAnn    :: !a
  , hookAttrs  :: ![AttributeGroup a]
  , hookFinal  :: !Bool
  , hookByRef  :: !Bool
  , hookType   :: !HookType
  , hookParam  :: !(Maybe (VarName a, Maybe (Type a)))
  , hookBody   :: !(HookBody a)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Property declaration in classes/traits.
data PropertyDecl a = PropertyDecl
  { propAnn       :: !a
  , propAttrs     :: ![AttributeGroup a]
  , propModifier  :: !PropertyModifier
  , propType      :: !(Maybe (Type a))
  , propItems     :: ![(VarName a, Maybe (Expr a))]
  , propHooks     :: ![PropertyHook a] -- ^ PHP 8.4 property hooks
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Class/Trait constant declaration (typed in PHP 8.3, in traits in PHP 8.2).
data ConstDecl a = ConstDecl
  { constAnn       :: !a
  , constAttrs     :: ![AttributeGroup a]
  , constVis       :: !(Maybe Visibility)
  , constFinal     :: !Bool
  , constType      :: !(Maybe (Type a)) -- ^ PHP 8.3 typed class constants
  , constItems     :: ![(Ident a, Expr a)]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Enum case declaration (PHP 8.1+).
data EnumCase a = EnumCase
  { enumCaseAnn   :: !a
  , enumCaseAttrs :: ![AttributeGroup a]
  , enumCaseName  :: !(Ident a)
  , enumCaseVal   :: !(Maybe (Expr a))
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Trait adaptation (as / insteadof).
data TraitAdaptation a
  = TraitAlias !a !(Maybe (QualifiedName a)) !(Ident a) !(Maybe Visibility) !(Maybe (Ident a))
  | TraitPrecedence !a !(QualifiedName a) !(Ident a) ![QualifiedName a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Trait use statement inside classes.
data TraitUse a = TraitUse
  { traitUseAnn         :: !a
  , traitUseNames       :: ![QualifiedName a]
  , traitUseAdaptations :: ![TraitAdaptation a]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Method parameter (supports constructor property promotion and asymmetric visibility).
data Param a = Param
  { paramAnn         :: !a
  , paramAttrs       :: ![AttributeGroup a]
  , paramVis         :: !(Maybe Visibility) -- ^ Constructor property promotion read vis
  , paramWriteVis    :: !(Maybe Visibility) -- ^ Asymmetric write vis (PHP 8.4)
  , paramReadonly    :: !Bool
  , paramType        :: !(Maybe (Type a))
  , paramByRef       :: !Bool
  , paramVariadic    :: !Bool
  , paramName        :: !(VarName a)
  , paramDefault     :: !(Maybe (Expr a))
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Method declaration in class/interface/trait.
data MethodDecl a = MethodDecl
  { methodAnn        :: !a
  , methodAttrs      :: ![AttributeGroup a]
  , methodModifier   :: !MethodModifier
  , methodByRef      :: !Bool
  , methodName       :: !(Ident a)
  , methodParams     :: ![Param a]
  , methodReturnType :: !(Maybe (Type a))
  , methodBody       :: !(Maybe [Stmt a])
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Class member declarations.
data ClassMember a
  = MemberProperty !(PropertyDecl a)
  | MemberMethod !(MethodDecl a)
  | MemberConst !(ConstDecl a)
  | MemberTraitUse !(TraitUse a)
  | MemberEnumCase !(EnumCase a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Class declaration.
data ClassDecl a = ClassDecl
  { classAnn        :: !a
  , classAttrs      :: ![AttributeGroup a]
  , classModifier   :: !ClassModifier
  , className       :: !(Ident a)
  , classExtends    :: !(Maybe (QualifiedName a))
  , classImplements :: ![QualifiedName a]
  , classMembers    :: ![ClassMember a]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Interface declaration.
data InterfaceDecl a = InterfaceDecl
  { ifaceAnn        :: !a
  , ifaceAttrs      :: ![AttributeGroup a]
  , ifaceName       :: !(Ident a)
  , ifaceExtends    :: ![QualifiedName a]
  , ifaceMembers    :: ![ClassMember a]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Trait declaration.
data TraitDecl a = TraitDecl
  { traitAnn        :: !a
  , traitAttrs      :: ![AttributeGroup a]
  , traitName       :: !(Ident a)
  , traitMembers    :: ![ClassMember a]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Enum declaration (pure or backed).
data EnumDecl a = EnumDecl
  { enumAnn         :: !a
  , enumAttrs       :: ![AttributeGroup a]
  , enumName        :: !(Ident a)
  , enumBackedType  :: !(Maybe (Type a))
  , enumImplements  :: ![QualifiedName a]
  , enumMembers     :: ![ClassMember a]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Function declaration.
data FunctionDecl a = FunctionDecl
  { funcAnn         :: !a
  , funcAttrs       :: ![AttributeGroup a]
  , funcByRef       :: !Bool
  , funcName        :: !(Ident a)
  , funcParams      :: ![Param a]
  , funcReturnType  :: !(Maybe (Type a))
  , funcBody        :: ![Stmt a]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Attribute group (#[Attr1, Attr2]).
data AttributeGroup a = AttributeGroup !a ![Attribute a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Attribute invocation.
data Attribute a = Attribute !a !(QualifiedName a) ![Arg a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Function / method / attribute argument.
data Arg a = Arg
  { argAnn    :: !a
  , argName   :: !(Maybe (Ident a)) -- ^ Named argument name: $val
  , argExpr   :: !(Expr a)
  , argUnpack :: !Bool              -- ^ ...$arg
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Target of a class operation (name or expression).
data ClassTarget a
  = ClassTargetName !(QualifiedName a)
  | ClassTargetExpr !(Expr a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Target of a class constant lookup (static identifier or dynamic expression).
data ClassConstName a
  = ConstNameIdent !(Ident a)
  | ConstNameDynamic !(Expr a) -- ^ PHP 8.3 dynamic class constant fetch: Class::{$var}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Property / method name (identifier or dynamic expression).
data MemberName a
  = MemberIdent !(Ident a)
  | MemberExpr !(Expr a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Call arguments or first-class callable placeholder (...).
data CallArgs a
  = ArgsList ![Arg a]
  | FirstClassCallable -- ^ PHP 8.1+ first-class callable: fn(...)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Match expression arm.
data MatchArm a
  = MatchArm !a ![Expr a] !(Expr a)
  | MatchDefault !a !(Expr a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Catch clause (supports non-capturing catch).
data CatchClause a = CatchClause
  { catchAnn   :: !a
  , catchTypes :: ![QualifiedName a]
  , catchVar   :: !(Maybe (VarName a)) -- ^ Nothing for non-capturing catch
  , catchBody  :: ![Stmt a]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Switch case.
data SwitchCase a
  = SwitchCase !a !(Expr a) ![Stmt a]
  | SwitchDefault !a ![Stmt a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Array element.
data ArrayItem a
  = ArrayItem
      { itemAnn    :: !a
      , itemKey    :: !(Maybe (Expr a))
      , itemValue  :: !(Expr a)
      , itemUnpack :: !Bool             -- ^ ...$arr
      }
  | ArrayItemEmpty
      { itemAnn    :: !a
      }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Binary operators.
data BinOp
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpPow
  | OpConcat
  | OpBitAnd
  | OpBitOr
  | OpBitXor
  | OpShiftLeft
  | OpShiftRight
  | OpEq
  | OpIdentical
  | OpNotEq
  | OpNotIdentical
  | OpLt
  | OpLte
  | OpGt
  | OpGte
  | OpSpaceship
  | OpBoolAnd
  | OpBoolOr
  | OpLogicalAnd
  | OpLogicalOr
  | OpLogicalXor
  | OpInstanceof
  | OpPipe -- ^ PHP 8.5 pipe operator (|>)
  | OpCoalesce -- ^ Null coalescing operator (??)
  deriving (Eq, Ord, Show, Generic)

-- | Unary operators.
data UnOp
  = OpPreInc
  | OpPostInc
  | OpPreDec
  | OpPostDec
  | OpUnaryPlus
  | OpUnaryMinus
  | OpBoolNot
  | OpBitNot
  | OpErrorSuppress
  deriving (Eq, Ord, Show, Generic)

-- | Cast types.
data CastType
  = CastInt
  | CastFloat
  | CastString
  | CastBool
  | CastArray
  | CastObject
  | CastUnset
  deriving (Eq, Ord, Show, Generic)

-- | Include types.
data IncludeType
  = IncInclude
  | IncIncludeOnce
  | IncRequire
  | IncRequireOnce
  deriving (Eq, Ord, Show, Generic)

-- | Which spelling of the script-termination construct was written.
data ExitKind
  = ExitExit
  | ExitDie
  deriving (Eq, Ord, Show, Generic)

-- | Use import type.
data UseType
  = UseNormal
  | UseFunction
  | UseConst
  deriving (Eq, Ord, Show, Generic)

-- | Use import clause.
data UseClause a = UseClause
  { useClauseAnn   :: !a
  , useClauseName  :: !(QualifiedName a)
  , useClauseAlias :: !(Maybe (Ident a))
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | String part for interpolated strings.
data StringPart a
  = StrLit !Text
  | StrExpr !(Expr a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Literals.
data Literal a
  = LitInt !a !Integer !Text     -- ^ Integer value + original text representation
  | LitFloat !a !Double !Text    -- ^ Float value + original text representation
  | LitString !a !Text !Text     -- ^ Unescaped value + original raw text
  | LitInterpolated !a ![StringPart a]
  | LitHeredoc !a !Text !Text !Bool -- ^ Label, content, isNowdoc
  | LitBool !a !Bool
  | LitNull !a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Variable representations (simple variable, variable-variable $$var, or dynamic variable ${expr}).
data Var a
  = SimpleVar !a !(VarName a)
  | DynamicVar !a !(Expr a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Expressions in PHP 8.2-8.5.
data Expr a
  = ExprVar !a !(Var a)
  | ExprLit !a !(Literal a)
  | ExprBinary !a !BinOp !(Expr a) !(Expr a)
  | ExprUnary !a !UnOp !(Expr a)
  | ExprAssign !a !(Maybe BinOp) !(Expr a) !(Expr a)
  | ExprAssignRef !a !(Expr a) !(Expr a) -- ^ By-reference assignment @$a =& $b@
  | ExprTernary !a !(Expr a) !(Maybe (Expr a)) !(Expr a)
  | ExprNullCoalesce !a !(Expr a) !(Expr a)
  | ExprClone !a !(Expr a) !(Maybe (Expr a)) -- ^ PHP 8.5 clone-with
  | ExprNew !a !(ClassTarget a) ![Arg a]
  | ExprNewAnonClass !a ![AttributeGroup a] !ClassModifier ![Arg a] !(Maybe (QualifiedName a)) ![QualifiedName a] ![ClassMember a]
  | ExprCall !a !(Expr a) !(CallArgs a)
  | ExprMethodCall !a !(Expr a) !(MemberName a) !(CallArgs a)
  | ExprNullsafeMethodCall !a !(Expr a) !(MemberName a) !(CallArgs a)
  | ExprPropertyFetch !a !(Expr a) !(MemberName a)
  | ExprNullsafePropertyFetch !a !(Expr a) !(MemberName a)
  | ExprStaticCall !a !(ClassTarget a) !(MemberName a) !(CallArgs a)
  | ExprStaticPropertyFetch !a !(ClassTarget a) !(VarName a)
  | ExprClassConstFetch !a !(ClassTarget a) !(ClassConstName a)
  | ExprArray !a ![ArrayItem a]
  | ExprList !a ![ArrayItem a]
  | ExprArrayAccess !a !(Expr a) !(Maybe (Expr a))
  | ExprMatch !a !(Expr a) ![MatchArm a]
  | ExprClosure !a ![AttributeGroup a] !Bool !Bool ![Param a] ![(VarName a, Bool)] !(Maybe (Type a)) ![Stmt a]
  | ExprArrowFunction !a ![AttributeGroup a] !Bool !Bool ![Param a] !(Maybe (Type a)) !(Expr a)
  | ExprYield !a !(Maybe (Expr a)) !(Maybe (Expr a))
  | ExprYieldFrom !a !(Expr a)
  | ExprCast !a !CastType !(Expr a)
  | ExprIsset !a ![Expr a]
  | ExprEmpty !a !(Expr a)
  | ExprEval !a !(Expr a)
  | ExprInclude !a !IncludeType !(Expr a)
  | ExprPrint !a !(Expr a)
  | ExprExit !a !ExitKind !(Maybe (Expr a))
  | ExprThrow !a !(Expr a)
  | ExprConstFetch !a !(QualifiedName a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Statements in PHP 8.2-8.5.
data Stmt a
  = StmtExpr !a !(Expr a)
  | StmtBlock !a ![Stmt a]
  | StmtIf !a !(Expr a) ![Stmt a] ![(Expr a, [Stmt a])] !(Maybe [Stmt a])
  | StmtWhile !a !(Expr a) ![Stmt a]
  | StmtDoWhile !a ![Stmt a] !(Expr a)
  | StmtFor !a ![Expr a] ![Expr a] ![Expr a] ![Stmt a]
  | StmtForeach !a !(Expr a) !(Maybe (Expr a)) !(Expr a) !Bool ![Stmt a]
  | StmtSwitch !a !(Expr a) ![SwitchCase a]
  | StmtBreak !a !(Maybe (Expr a))
  | StmtContinue !a !(Maybe (Expr a))
  | StmtReturn !a !(Maybe (Expr a))
  | StmtThrowStmt !a !(Expr a)
  | StmtTry !a ![Stmt a] ![CatchClause a] !(Maybe [Stmt a])
  | StmtNamespace !a !(Maybe (QualifiedName a)) !(Maybe [Stmt a]) -- ^ Nothing if unbracketed till EOF
  | StmtUse !a !UseType ![UseClause a]
  | StmtGroupUse !a !UseType !(QualifiedName a) ![UseClause a]
  | StmtConst !a !(ConstDecl a)
  | StmtFunction !a !(FunctionDecl a)
  | StmtClass !a !(ClassDecl a)
  | StmtInterface !a !(InterfaceDecl a)
  | StmtTrait !a !(TraitDecl a)
  | StmtEnum !a !(EnumDecl a)
  | StmtEcho !a ![Expr a]
  | StmtGlobal !a ![Expr a]
  | StmtStatic !a ![(VarName a, Maybe (Expr a))]
  | StmtDeclare !a ![DeclareDirective a] !(Maybe [Stmt a])
  | StmtGoto !a !(Ident a)
  | StmtLabel !a !(Ident a)
  | StmtUnset !a ![Expr a]
  | StmtInlineHtml !a !Text
  | StmtHaltCompiler !a !Text
  | StmtEmpty !a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Declare directive (e.g. strict_types=1, ticks=1, encoding='UTF-8').
data DeclareDirective a = DeclareDirective
  { declareDirectiveAnn   :: !a
  , declareDirectiveName  :: !(Ident a)
  , declareDirectiveValue :: !(Literal a)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Top-level PHP program.
data Program a = Program !a ![Stmt a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- | Helper to extract annotation from any AST node.
getAnnotation :: Expr a -> a
getAnnotation = \case
  ExprVar a _                  -> a
  ExprLit a _                  -> a
  ExprBinary a _ _ _           -> a
  ExprUnary a _ _              -> a
  ExprAssign a _ _ _           -> a
  ExprAssignRef a _ _          -> a
  ExprTernary a _ _ _          -> a
  ExprNullCoalesce a _ _       -> a
  ExprClone a _ _              -> a
  ExprNew a _ _                -> a
  ExprNewAnonClass a _ _ _ _ _ _ -> a
  ExprCall a _ _               -> a
  ExprMethodCall a _ _ _       -> a
  ExprNullsafeMethodCall a _ _ _ -> a
  ExprPropertyFetch a _ _      -> a
  ExprNullsafePropertyFetch a _ _ -> a
  ExprStaticCall a _ _ _       -> a
  ExprStaticPropertyFetch a _ _ -> a
  ExprClassConstFetch a _ _    -> a
  ExprArray a _                -> a
  ExprList a _                 -> a
  ExprArrayAccess a _ _        -> a
  ExprMatch a _ _              -> a
  ExprClosure a _ _ _ _ _ _ _  -> a
  ExprArrowFunction a _ _ _ _ _ _ -> a
  ExprYield a _ _              -> a
  ExprYieldFrom a _            -> a
  ExprCast a _ _               -> a
  ExprIsset a _                -> a
  ExprEmpty a _                -> a
  ExprEval a _                 -> a
  ExprInclude a _ _            -> a
  ExprPrint a _                -> a
  ExprExit a _ _               -> a
  ExprThrow a _                -> a
  ExprConstFetch a _           -> a
