module Main where

import CommonPrefix
import Control.Monad
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Util as PP
import qualified Data.Set as S

import Prelude hiding (seq)

import Debug.Trace

-- This program takes the C grammar as defined in the standard and
-- removes epsilon rules and left recursion.  Leaving something
-- suitable to be implemented in a recursive descent parser.
--

-- We have two representations of the grammar.  This first one makes it
-- easy to write down the grammar, since rules directly refer to other
-- rules via their Haskell bindings.  But this builds a cyclic graph,
-- which is harder for us to process since we'd need to keep passing
-- around a 'seen' set to avoid processing the same production more
-- than once.
data Lex = Literal String |
           Token String
    deriving (Eq, Ord, Show)

instance PP.Pretty Lex where
    pretty (Literal str) = PP.pretty "\"" <> PP.pretty str <> PP.pretty "\""
    pretty (Token str) = PP.pretty str

data Rule_ = Terminal_ Lex |
             NonTerminal_ String Rule_ |
             Seq_ [Rule_] |
             Alt_ [Rule_] |
             Epsilon_
        deriving (Eq, Show)

-- so we can change the representation later
nonTerminal = NonTerminal_

seq  = Seq_
alt = Alt_
epsilon = Epsilon_

lit :: String -> Rule_
lit = Terminal_ . Literal

token :: String -> Rule_
token = Terminal_ . Token

lits :: [String] -> Rule_
lits = alt . map lit

opt x = alt [x, epsilon]
comma = lit ","

identifier = token "IDENTIFIER"
constant = token "CONSTANT"
stringLiteral = token "STRING-LITERAL"

---------------
-- Expressions
--
primaryExpression = nonTerminal "primary-expression" $
    alt [identifier,
         constant,
         stringLiteral,
         seq [lit "(", expression, lit ")"],
         genericSelection]

genericSelection = nonTerminal "generic-selection" $
    seq [lit "_Generic",
         lit "(",
         assignmentExpression,
         comma,
         genericAssocList,
         lit ")"
         ]

genericAssocList = nonTerminal "generic-assoc-list" $
    alt [genericAssociation,
         seq [genericAssocList, comma, genericAssociation]
         ]

genericAssociation = nonTerminal "generic-association" $
    alt [seq [typeName, lit ":", assignmentExpression],
         seq [lit "default", lit ":", assignmentExpression]
         ]

postfixExpression = nonTerminal "postfix-expression" $
    alt [primaryExpression,
         seq [postfixExpression, lit "[", expression, lit "]"],
         seq [postfixExpression, lit "(", opt argumentExpressionList, lit ")"],
         seq [postfixExpression, lit ".", identifier],
         seq [postfixExpression, lit "->", identifier],
         seq [postfixExpression, lit "++"],
         seq [postfixExpression, lit "--"],
         seq [lit "(", typeName, lit ")", lit "{", initializerList, opt comma, lit "}"]
         ]

argumentExpressionList = nonTerminal "argumentExpressionList" $
    alt [assignmentExpression,
         seq [argumentExpressionList, comma, assignmentExpression]
         ]

unaryExpression = nonTerminal "unary-expression" $
    alt [postfixExpression,
         seq [lit "++", unaryExpression],
         seq [lit "--", unaryExpression],
         seq [unaryOperator, castExpression],
         seq [lit "sizeof", unaryExpression],
         seq [lit "sizeof", lit "(", typeName, lit ")"],
         seq [lit "_Alignof", lit "(", typeName, lit ")"]
         ]

unaryOperator = nonTerminal "unary-operator" $
    lits ["&", "*", "+", "-", "~", "!"]

castExpression = nonTerminal "cast-expression" $
    alt [unaryExpression,
         seq [lit "(", typeName, lit ")", castExpression]
         ]

multiplicativeExpression = nonTerminal "multiplicative-expression" $
    alt [castExpression,
         seq [multiplicativeExpression, lit "*", castExpression],
         seq [multiplicativeExpression, lit "/", castExpression],
         seq [multiplicativeExpression, lit "%", castExpression]
         ]

additiveExpression = nonTerminal "additive-expression" $
    alt [multiplicativeExpression,
         seq [additiveExpression, lit "+", multiplicativeExpression],
         seq [additiveExpression, lit "-", multiplicativeExpression]
         ]

shiftExpression = nonTerminal "shift-expression" $
    alt [additiveExpression,
         seq [shiftExpression, lit "<<", additiveExpression],
         seq [shiftExpression, lit ">>", additiveExpression]
         ]

relationalExpression = nonTerminal "relational-expression" $
    alt [shiftExpression,
         seq [relationalExpression, lit "<", shiftExpression],
         seq [relationalExpression, lit ">", shiftExpression],
         seq [relationalExpression, lit "<=", shiftExpression],
         seq [relationalExpression, lit ">=", shiftExpression]
         ]

equalityExpression = nonTerminal "equality-expression" $
    alt [relationalExpression,
         seq [equalityExpression, lit "==", relationalExpression],
         seq [equalityExpression, lit "!=", relationalExpression]
         ]

andExpression = nonTerminal "and-expression" $
    alt [equalityExpression,
         seq [andExpression, lit "&", equalityExpression]
         ]

exclusiveOrExpression = nonTerminal "exclusive-or-expression" $
    alt [andExpression,
         seq [exclusiveOrExpression, lit "^", andExpression]
         ]

inclusiveOrExpression = nonTerminal "inclusive-or-expression" $
    alt [exclusiveOrExpression,
         seq [inclusiveOrExpression, lit "|", exclusiveOrExpression]
         ]

logicalAndExpression = nonTerminal "logical-and-expression" $
    alt [inclusiveOrExpression,
         seq [logicalAndExpression, lit "&&", inclusiveOrExpression]
         ]

logicalOrExpression = nonTerminal "logical-or-expression" $
    alt [logicalAndExpression,
         seq [logicalOrExpression, lit "||", logicalAndExpression]
         ]

conditionalExpression = nonTerminal "conditional-expression" $
    alt [logicalOrExpression,
         seq [logicalOrExpression, lit "?", expression, lit ":", conditionalExpression]
         ]

assignmentExpression = nonTerminal "assignment-expression" $
    alt [conditionalExpression,
         seq [unaryExpression, assignmentOperator, assignmentExpression]
         ]

assignmentOperator = nonTerminal "assignment-operator" $
    lits ["=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|="]

expression = nonTerminal "expression" $
    alt [assignmentExpression,
         seq [expression, comma, assignmentExpression]
         ]

constantExpression = nonTerminal "constant-expression" conditionalExpression

----------------
-- Declarations
--

declaration = nonTerminal "declaration" $
    alt [seq [declarationSpecifiers, opt initDeclaratorList],
         staticAssertDeclaration
         ]

declarationSpecifiers = nonTerminal "declaration-specifiers" $
    alt [seq [storageClassSpecifier, opt declarationSpecifiers],
         seq [typeSpecifier, opt declarationSpecifiers],
         seq [typeQualifier, opt declarationSpecifiers],
         seq [functionSpecifier, opt declarationSpecifiers],
         seq [alignmentSpecifier, opt declarationSpecifiers]
         ]

initDeclaratorList = nonTerminal "init-declarator-list" $
    alt [initDeclarator,
         seq [initDeclaratorList, comma, initDeclarator]
         ]

initDeclarator = nonTerminal "init-declarator" $
    alt [declarator,
         seq [declarator, lit "=", initializer]
         ]

storageClassSpecifier = nonTerminal "storage-class-specifier" $
    lits ["typedef", "extern", "static", "_Thread_local", "auto", "register"]

typeSpecifier = nonTerminal "type-specifier" $
    alt [lits ["void", "char", "short", "int", "long", "float",
               "double", "signed", "unsigned", "_Bool", "_Complex"],
         atomicTypeSpecifier,
         structOrUnionSpecifier,
         enumSpecifier,
         typedefName
         ]

structOrUnionSpecifier = nonTerminal "struct-or-union-specifier" $
    alt [seq [structOrUnion, opt identifier, lit "{", structDeclarationList, lit "}"],
         seq [structOrUnion, identifier]
         ]

structOrUnion = nonTerminal "struct-or-union" $ lits ["struct", "union"]

structDeclarationList = nonTerminal "struct-declaration-list" $
    alt [structDeclaration,
         seq [structDeclarationList, structDeclaration]
         ]

structDeclaration = nonTerminal "struct-declaration" $
    alt [seq [specifierQualifierList, opt structDeclaratorList, lit ";"],
         staticAssertDeclaration
         ]

specifierQualifierList = nonTerminal "specifier-qualifier-list" $
    alt [seq [typeSpecifier, opt specifierQualifierList],
         seq [typeQualifier, opt specifierQualifierList]
         ]

structDeclaratorList = nonTerminal "struct-declarator-list" $
    alt [structDeclarator,
         seq [structDeclaratorList, comma, structDeclarator]
         ]

structDeclarator = nonTerminal "struct-declarator" $
    alt [declarator,
         seq [opt declarator, lit ":", constantExpression]
         ]

enum = lit "enum"

enumSpecifier = nonTerminal "enum-specifier" $
    alt [seq [enum, opt identifier, lit "{", enumeratorList, opt comma, lit "}"],
         seq [enum, identifier]
         ]

enumeratorList = nonTerminal "enumerator-list" $
    alt [enumerator,
         seq [enumeratorList, comma, enumerator]
         ]

enumerationConstant = identifier
enumerator = nonTerminal "enumerator" $
    alt [enumerationConstant,
         seq [enumerationConstant, lit "=", constantExpression]
         ]

atomicTypeSpecifier = nonTerminal "atomic-type-specifier" $
    seq [lit "_Atomic", lit "(", typeName, lit ")"]

typeQualifier = nonTerminal "type-qualifier" $
    lits ["const", "restrict", "volatile", "_Atomic"]

functionSpecifier = nonTerminal "function-specifier" $
    lits ["inline", "_Noreturn"]

alignmentSpecifier = nonTerminal "alignment-specifier" $
    alt [seq [lit "_Alignas", lit "(", typeName, lit ")"],
         seq [lit "_Alignas", lit "(", constantExpression, lit ")"]
         ]

declarator = nonTerminal "declarator" $
    seq [opt pointer, directDeclarator]


directDeclarator = nonTerminal "direct-declarator" $
    alt [identifier,
         seq [lit "(", declarator, lit ")"],
         seq [directDeclarator, lit "[", opt typeQualifierList, opt assignmentExpression, lit "]"],
         seq [directDeclarator, lit "[", lit "static", opt typeQualifierList, assignmentExpression, lit "]"],
         seq [directDeclarator, lit "[", typeQualifierList, lit "static", assignmentExpression, lit "]"],
         seq [directDeclarator, lit "[", opt typeQualifierList, lit "*", lit "]"],
         seq [directDeclarator, lit "(", parameterTypeList, lit ")"],
         seq [directDeclarator, lit "(", opt identifierList, lit ")"]
         ]

pointer = nonTerminal "pointer" $
    alt [seq [lit "*", opt typeQualifierList],
         seq [lit "*", opt typeQualifierList, pointer]
         ]

typeQualifierList = nonTerminal "type-qualifier-list" $
    alt [typeQualifier,
         seq [typeQualifierList, typeQualifier]
         ]

parameterTypeList = nonTerminal "parameter-type-list" $
    alt [parameterList,
         seq [parameterList, comma, lit "..."]
         ]

parameterList = nonTerminal "parameter-list" $
    alt [parameterDeclaration,
         seq [parameterList, comma, parameterDeclaration]
         ]

parameterDeclaration = nonTerminal "parameter-declaration" $
    alt [seq [declarationSpecifiers, declarator],
         seq [declarationSpecifiers, opt abstractDeclarator]
         ]

identifierList = nonTerminal "identifier-list" $
    alt [identifier,
         seq [identifierList, comma, identifier]
         ]

typeName = nonTerminal "type-name" $
    seq [specifierQualifierList, opt abstractDeclarator]

abstractDeclarator = nonTerminal "abstract-declarator" $
    alt [pointer, seq [opt pointer, directAbstractDeclarator]]

directAbstractDeclarator = nonTerminal "direct-abstract-declarator" $
    alt [seq [lit "(", abstractDeclarator, lit ")"],
         seq [opt directAbstractDeclarator, lit "[", opt typeQualifierList, opt assignmentExpression, lit "]"],
         seq [opt directAbstractDeclarator, lit "[", lit "static", opt typeQualifierList, assignmentExpression, lit "]"],
         seq [opt directAbstractDeclarator, lit "[", typeQualifierList, lit "static", assignmentExpression, lit "]"],
         seq [opt directAbstractDeclarator, lit "[", lit "*", lit "]"],
         seq [opt directAbstractDeclarator, lit "(", opt parameterTypeList, lit ")"]
         ]

typedefName = nonTerminal "typedef-name" identifier

initializer = nonTerminal "initializer" $
    alt [assignmentExpression,
         seq [lit "{", initializerList, opt comma, lit "}"]
         ]

initializerList = nonTerminal "initializer-list" $
    alt [seq [opt designation, initializer],
         seq [initializerList, comma, opt designation, initializer]
         ]

designation = nonTerminal "designation" $
    seq [designatorList, lit "="]

designatorList = nonTerminal "designation-list" $
    alt [designator, seq [designatorList, designator]]

designator = nonTerminal "designator" $
    alt [seq [lit "[", constantExpression, lit "]"],
         seq [lit ".", identifier]
         ]

staticAssertDeclaration = nonTerminal "static-assert-declaration" $
    seq [lit "_Static_assert", lit "(", constantExpression, comma, stringLiteral, lit ")", lit ";"]

--------------
-- Statements
--
statement = nonTerminal "statement" $
    alt [labeledStatement,
         compoundStatement,
         expressionStatement,
         selectionStatement,
         iterationStatement,
         jumpStatement]

labeledStatement = nonTerminal "labeled-statement" $
    alt [seq [identifier, lit ":", statement],
         seq [lit "case", constantExpression, lit ":", statement],
         seq [lit "default", lit ":", statement]
         ]

compoundStatement = nonTerminal "compound-statement" $
    seq [lit "{", opt blockItemList, lit "}"]

blockItemList = nonTerminal "block-item-list" $
    alt [blockItem, seq [blockItemList, blockItem]]

blockItem = nonTerminal "block-item" $
    alt [declaration, statement]

expressionStatement = nonTerminal "expression-statement" $
    seq [opt expression, lit ";"]

selectionStatement = nonTerminal "selection-statement" $
    alt [seq [lit "if", lit "(", expression, lit ")", statement],
         seq [lit "if", lit "(", expression, lit ")", statement, lit "else", statement],
         seq [lit "switch", lit "(", expression, lit ")", statement]
         ]

iterationStatement = nonTerminal "iteration-statement" $
    alt [seq [lit "while", lit "(", expression, lit ")", statement],
         seq [lit "do", statement, lit "while", lit "(", expression, lit ")", lit ":"],
         seq [lit "for", lit "(", opt expression, lit ";", opt expression, lit ";", opt expression, lit ")", statement],
         seq [lit "for", lit "(", declaration, opt expression, lit ";", opt expression, lit ")", statement]
         ]

jumpStatement = nonTerminal "jump-statement" $
    alt [seq [lit "goto", identifier, lit ";"],
         seq [lit "continue", lit ";"],
         seq [lit "break", lit ";"],
         seq [lit "return", opt expression, lit ";"]
         ]

------------------------
-- External definitions
--
translationUnit = nonTerminal "translation-unit" $
    alt [externalDeclaration,
         seq [translationUnit, externalDeclaration]
         ]

externalDeclaration = nonTerminal "external-declaration" $
    alt [functionDefinition, declaration]

functionDefinition = nonTerminal "function-definition" $
    seq [declarationSpecifiers, declarator, opt declarationList, compoundStatement]

declarationList = nonTerminal "declaration-list" $
    alt [declaration, seq [declarationList, declaration]]

----------------------------
-- Now for representation 2
--
data Identifier = Identifier [String]
    deriving (Eq, Show, Ord)

mkId :: String -> Identifier
mkId = Identifier . (: [])

extendId :: String -> Identifier -> Identifier
extendId nm (Identifier nms) = Identifier $ nm : nms

instance PP.Pretty Identifier where
    pretty (Identifier nm) = PP.pretty . concat . intersperse "-" . reverse $ nm

data Rule = Terminal Lex |
            NonTerminal Identifier |
            Alt [Rule] |
            Seq [Rule] |
            Epsilon
    deriving (Eq, Show)

type Production = (Identifier, Rule)
type Grammar = Map Identifier Rule

nonTerminals :: Rule_ -> Grammar
nonTerminals g = scan g M.empty
    where
        scan :: Rule_ -> Grammar -> Grammar
        scan (Terminal_ _) acc = acc
        scan g@(NonTerminal_ nm g') acc =
            case M.lookup (mkId nm) acc of
                Just _ -> acc
                Nothing -> scan g' $ M.insert (mkId nm) (convertRule g') acc
        scan (Seq_ gs) acc = foldr scan acc gs
        scan (Alt_ gs) acc = foldr scan acc gs
        scan Epsilon_ acc = acc

        convertRule (Terminal_ t) = Terminal t
        convertRule (NonTerminal_ t _) = NonTerminal (Identifier [t])
        convertRule (Alt_ gs) = Alt $ map convertRule gs
        convertRule (Seq_ gs) = Seq $ map convertRule gs
        convertRule Epsilon_ = Epsilon

-------------
-- Top level
--

-- repeatedly apply a function until there's no change
stabilise :: (Eq a) => (a -> a) -> a -> a
stabilise fn x = if x == x'
    then x
    else stabilise fn x'
    where
        x' = fn x

-- FIXME: collapse Alt [x1 ... (Alt [y1 ... yn]) ... xn] -> Alt [x1 ... xn, y1 .. yn]

-- The refactoring is easier if we force every non terminal to
-- be of the form (Alt (Seq ...) (Seq ...) ...).  This may
-- require adding new intermediate non terminals.
normalise :: Grammar -> Grammar
normalise = M.map (stabilise norm)
    where
        norm = stripEpsilonSeq . collapseAltAlt . liftAlt . rmSingletons

        stripEpsilonSeq (Seq gs) =
            case filter (not . isEpsilon) gs of
                [] -> Epsilon
                gs' -> Seq (map stripEpsilonSeq gs')
        stripEpsilonSeq (Alt gs) = Alt $ map stripEpsilonSeq gs
        stripEpsilonSeq r = r

        collapseAltAlt (Seq gs) = Seq $ collapseAlts gs
        collapseAltAlt (Alt gs) =
            case partition isAlt (collapseAlts gs) of
                (alts, notAlts) -> Alt $ (collapseAlts notAlts) ++
                      (concatMap unwrap (collapseAlts alts))
            where
                unwrap (Alt gs) = gs
                unwrap _ = error "not an alt"
        collapseAltAlt g = g

        collapseAlts = map collapseAltAlt

        -- Seq [... Alt [a1, a2 ...] -> Alt [Seq [... a1 ...], Seq [... a2 ...]]
        liftAlt (Seq gs) = case break isAlt gs of
            (_, []) -> Seq $ map liftAlt gs
            (prefix, xs:postfix) -> Alt (map (\x -> Seq $ prefix ++ [x] ++ postfix) (getAlts xs))
        liftAlt (Alt gs) = Alt (map liftAlt gs)
        liftAlt g = g

        rmSingletons (Seq [g]) = rmSingletons g
        rmSingletons (Seq gs) = Seq $ map rmSingletons gs
        rmSingletons (Alt [g]) = rmSingletons g
        rmSingletons (Alt gs) = Alt $ map rmSingletons gs
        rmSingletons g = g

        isEpsilon Epsilon = True
        isEpsilon _ = False

        isAlt (Alt _) = True
        isAlt _ = False

        getAlts (Alt xs) = xs
        getAlts _ = []

-----------------------------------------
-- Now that we've normalised we can change rep
-- once again to force it to stay normalised.

data Elt = Terminal' Lex | NonTerminal' Identifier deriving (Eq, Ord, Show)

instance PP.Pretty Elt where
    pretty (Terminal' l) = PP.pretty l
    pretty (NonTerminal' nm) = PP.pretty nm

-- Rules are now forced to be an implicit Alt of Seqs
data Rule' = Rule' [[Elt]] deriving (Eq, Show)

type Production' = (Identifier, Rule')
type Grammar' = Map Identifier Rule'

toRep3 :: Grammar -> Grammar'
toRep3 = M.fromList . map convert . M.toList
    where
        convert :: Production -> Production'
        convert (nm, r) = (nm, convertRule r)

        convertRule :: Rule -> Rule'
        convertRule (Alt seqs) = Rule' $ map convertSeq seqs
        convertRule (Seq xs) = Rule' [map convertElt xs]
        convertRule (Terminal l) = Rule' [[Terminal' l]]
        convertRule (NonTerminal nm') = Rule' [[NonTerminal' nm']]

        convertSeq :: Rule -> [Elt]
        convertSeq (Terminal l) = [Terminal' l]
        convertSeq (NonTerminal nm) = [NonTerminal' nm]
        convertSeq (Seq gs) = map convertElt gs
        convertSeq (Alt _) = error "nested Alt"
        convertSeq Epsilon = error "epsilon still present"

        convertElt :: Rule -> Elt
        convertElt (Terminal l) = Terminal' l
        convertElt (NonTerminal nm) = NonTerminal' nm
        convertElt _ = error "not an element"

-- The rule shold be in normal form, ie. an Alt of Seqs, and
-- no deeper
elimImmediateRecursion' :: Production' -> [Production']
elimImmediateRecursion' p@(nm, Rule' gs) =
    case partition (beginsWith nm) gs of
        ([], _) -> [p]
        (gs1, gs2) -> [(nmHead, Rule' gs2),
                       (nmTail, Rule' $ map tail gs1),
                       (nmTails, Rule' $ [[NonTerminal' nmTail, NonTerminal' nmTails],
                                          [NonTerminal' nmTail]]),
                       (nm, Rule' [[NonTerminal' nmHead, NonTerminal' nmTails],
                                   [NonTerminal' nmHead]])]
    where
        nmHead = extendId "head" nm
        nmTail = extendId "tail" nm
        nmTails = extendId "tails" nm

        beginsWith :: Identifier -> [Elt] -> Bool
        beginsWith nm ((NonTerminal' nm'):_) = nm == nm'
        beginsWith _ _ = False

elimImmediateRecursion :: Grammar' -> Grammar'
elimImmediateRecursion =
    M.fromList .
    concatMap elimImmediateRecursion' .
    M.toList

uniq = S.toList . S.fromList

references :: Identifier -> Grammar' -> [Identifier]
references nm g = case M.lookup nm g of
    Just (Rule' rs) -> uniq . concatMap getId . concat $ rs
    Nothing -> error "couldn't find identifier in grammar"
    where
        getId :: Elt -> [Identifier]
        getId (NonTerminal' nm) = [nm]
        getId _ = []

-- Grabs the identifiers from the botton up
getIdentifiers :: Grammar' -> Identifier -> [Identifier]
getIdentifiers g nm = loop [nm] [] (S.singleton nm)
    where
        loop :: [Identifier] -> [Identifier] -> Set Identifier -> [Identifier]
        loop [] acc seen = acc
        loop (x:xs) acc seen = loop (xs ++ unseen) (x:acc) (foldr S.insert seen unseen)
            where
                rs = references x g
                unseen = filter (not . (\nm -> S.member nm seen)) rs

elimRecursion :: Grammar' -> Grammar'
elimRecursion g = foldr elim g pairs
    where
        -- FIXME: ordering is important for this
        nms = getIdentifiers g start

        pairs :: [(Identifier, [Identifier])]
        pairs = map (\(x:xs) -> (x, xs)) . drop 1 . reverse . tails . reverse $ nms

        elim :: (Identifier, [Identifier]) -> Grammar' -> Grammar'
        elim (i, js) g = foldr (subst i) g js

        subst :: Identifier -> Identifier -> Grammar' -> Grammar'
        subst i j g = case M.lookup i g of
            Just (Rule' gs) -> M.insert i (Rule' $ concatMap (expand j) gs) g
            Nothing -> error "couldn't find non terminal in grammar"

        expand :: Identifier -> [Elt] -> [[Elt]]
        expand j ((NonTerminal' nm):xs) | nm == j =
            case M.lookup j g of
                Just (Rule' js) -> map (++ xs) js
                Nothing -> error "couldn't find non terminal in grammar"
        expand j elts = [elts]

-- Unit productions have the form A -> B, so every use
-- of A can be replaced with B, eliminating A
elimUnits :: Grammar' -> Grammar'
elimUnits g = foldr (\p g -> M.map ((uncurry replace) p) g) g units
    where
        units = concatMap findUnits . M.toList $ g

        findUnits :: Production' -> [(Identifier, Identifier)]
        findUnits (x, Rule' [[(NonTerminal' y)]]) = [(x, y)]
        findUnits _ = []

        replace :: Identifier -> Identifier -> Rule' -> Rule'
        replace old new (Rule' gs) = Rule' $ map (map (replaceElt old new)) gs

        replaceElt :: Identifier -> Identifier -> Elt -> Elt
        replaceElt old new e@(NonTerminal' nm) | old == nm = NonTerminal' new
        replaceElt _ _ r = r

elimUnreferenced :: Identifier -> Grammar' -> Grammar'
elimUnreferenced top g = foldr copy M.empty $ getIdentifiers g top
    where
        copy nm g' = case M.lookup nm g of
            Just r -> M.insert nm r g'
            Nothing -> error "couldn't find non terminal in grammar"

--------------------------------------------
-- Left factor

factor :: Production' -> [Production']
factor p@(nm, Rule' xs) =
    if null subs then [p] else (newParent : children)
    where
        names :: [Identifier]
        names = map (\n -> extendId n nm) (map show [1..])

        subs :: [(Identifier, ([Elt], [[Elt]]))]
        subs = zip names $ commonPrefix xs

        newParent = (nm, Rule' $ map parentProduction subs)

        parentProduction (newNm, (prefix, tails)) =
            if length tails == 1
                then prefix ++ head tails
                else prefix ++ [NonTerminal' newNm]

        children :: [Production']
        children = concatMap child subs

        child (newNm, (prefix, tails)) =
            if length tails == 1
                then []
                else [(newNm, Rule' tails)]

leftFactor :: Grammar' -> Grammar'
leftFactor = M.fromList . concatMap factor . M.toList

--------------------------------------------------------

firstPos :: Identifier -> Grammar' -> Set Lex
firstPos nm g = case M.lookup nm g of
    Just (Rule' xs) -> foldr scan S.empty xs
    Nothing -> error "couldn't find non terminal in grammar"
    where
        scan [] s = s
        scan ((NonTerminal' nm'):_) s = S.union s (firstPos nm' g)
        scan ((Terminal' l):_) s = S.insert l s

start :: Identifier
start = mkId "translation-unit"

cGrammar :: Grammar'
cGrammar =
    elimUnreferenced start .
    elimUnits .
    leftFactor .
    elimUnreferenced start .
    elimUnits .
    elimImmediateRecursion .
    elimRecursion .
    elimUnreferenced start .
    elimUnits .
    elimImmediateRecursion .
    toRep3 .
    normalise .
    nonTerminals $
    translationUnit

type Doc = PP.Doc ()

showRule :: Rule' -> Doc
showRule (Rule' gs) = PP.sep . PP.punctuate (PP.pretty " |") . map showSeq $ gs
    where
        showSeq [] = PP.pretty "epsilon"
        showSeq xs = PP.hsep . map PP.pretty $ xs

showRules :: Grammar' -> Identifier -> Doc
showRules g top = PP.sep $ map format nms
    where
        nms = getIdentifiers g top

        format :: Identifier -> Doc
        format nm = case M.lookup nm g of
            Just r -> PP.pretty nm PP.<> PP.pretty ": " PP.<> PP.line PP.<> (PP.indent 4 $ showRule r) PP.<> PP.line
            Nothing -> error "couldn't lookup non terminal in grammar"

showFirstPos :: Grammar' -> Identifier -> Doc
showFirstPos g top = PP.sep $ map (\nm -> PP.pretty nm PP.<> PP.pretty ": " PP.<> PP.prettyList (S.toList $ firstPos nm g)) nms
    where
        nms = getIdentifiers g top

main = do
    PP.putDocW 120 $ showRules cGrammar start
    -- PP.putDocW 120 $ showFirstPos cGrammar start

traceIt :: (Show a) => a -> a
traceIt x = trace (show x) x
