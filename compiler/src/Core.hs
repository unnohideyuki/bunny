module Core where

import Symbol
import Text.PrettyPrint.ANSI.Leijen

data Module = Module Id [TypeDefn] [ValueDefn]

data TypeDefn = DataDefn Id [TypeBinder] [ConstrDefn]
              | NewtypeDefn Id Id [TypeBinder] Type

data ConstrDefn = ConstrDefn Id [TypeBinder] [AtomType]

data ValueDefn = ValueDefn Id Type Expr

data AtomExpr = VarExpr Id
              | ConExpr Id
              | LitExpr Literal
              | ParExpr Expr

data Expr = AExpr AtomExpr
          | AppExpr AtomExpr [Arg]
          | LamExpr [Binder] Expr
          | LetExpr [ValueDefn] Expr
          | CaseExpr AtomType Expr ValueBinder [CaseAlt]
          | CastExpr Expr AtomType

data Arg = TypeArg AtomType
         | ValueArg AtomExpr

data CaseAlt = ConAlt Id [TypeBinder] [ValueBinder] Expr
             | LitAlt Literal Expr
             | DefaultAlt Expr

data Binder = TBind TypeBinder
            | VBind ValueBinder

data TypeBinder = TypeBinder Id Kind

data ValueBinder = ValueBinder Id Type

data Literal = LitInt  Integer  Type
             | LitChar Char     Type
             | LitRat  Rational Type
             | LitStr  String   Type

data AtomType = Tyvar Id
              | Tycon Id
              | ParType Type

data BasicType = AType AtomType
               | AppType BasicType AtomType

data Type = BType BasicType
          | TypeScheme [TypeBinder] Type
          | ArrType BasicType Type

data AtomKind = Star
              | ParKind Kind

data Kind = AKind AtomKind
          | ArrKind AtomKind Kind


ppModule :: Module -> Doc
ppModule (Module modident _ _) =
  text "Module" <+> text modident <> line