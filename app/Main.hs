module Main where

import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Util as PP
import qualified Data.Set as S

import Prelude hiding (seq)

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
data Rule_ = Terminal_ String |
             NonTerminal_ String Rule_ |
             Seq_ [Rule_] |
             Alt_ [Rule_] |
             Epsilon_
        deriving (Eq, Show)

-- so we can change the representation later
terminal = Terminal_
nonTerminal = NonTerminal_

seq  = Seq_
alt = Alt_
epsilon = Epsilon_

lit :: String -> Rule_
lit = terminal

lits :: [String] -> Rule_
lits = alt . map lit

opt x = alt [x, epsilon]
comma = terminal ","

identifier = terminal "<identifier>"
constant = terminal "<constant>"
stringLiteral = terminal "<string-literal>"

---------------
-- Expressions
--
primaryExpression = nonTerminal "PrimaryExpression" $
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
         seq [directDeclarator, lit "[", lit "static", opt typeQualifierList, assignmentExpression],
         seq [directDeclarator, lit "[", typeQualifierList, lit "static", assignmentExpression],
         seq [directDeclarator, lit "[", opt typeQualifierList, lit "*"],
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
data Rule = Terminal String |
            NonTerminal String |
            Alt [Rule] |
            Seq [Rule] |
            Epsilon
    deriving (Eq, Show)

type Grammar = Map String Rule

nonTerminals :: Rule_ -> Grammar
nonTerminals g = scan g M.empty
    where
        scan :: Rule_ -> Grammar -> Grammar
        scan (Terminal_ _) acc = acc
        scan g@(NonTerminal_ nm g') acc =
            case M.lookup nm acc of
                Just _ -> acc
                Nothing -> scan g' $ M.insert nm (convertRule g') acc
        scan (Seq_ gs) acc = foldr scan acc gs
        scan (Alt_ gs) acc = foldr scan acc gs
        scan Epsilon_ acc = acc

        convertRule (Terminal_ t) = Terminal t
        convertRule (NonTerminal_ t _) = NonTerminal t
        convertRule (Alt_ gs) = Alt $ map convertRule gs
        convertRule (Seq_ gs) = Seq $ map convertRule gs
        convertRule Epsilon_ = Epsilon

-------------
-- Top level
--

-- Remove epsilons
-- 1) find a non-terminal that accepts epsilon (A)
-- 2) remove that non terminal, and introduce another similar one (A') that doesn't have the epsilon production
-- 3) replace every rule, g, that uses A with (Alt (subst A A' g) (remove A g))
-- 4) repeat

{-
substWithEpsilon :: Rule_ -> String -> Rule_
substWithEpsilon (Seq g1@(NonTerminal nt _) g2) nm =
    if nm == nt
        then Alt (Seq g1 g2') g2'
        else Seq g1 g2'
    where
        g2' = substWithEpsilon g2 nm
substWithEpsilon (Alt g1 g2) nm =
    Alt (substWithEpsilon g1 nm) (substWithEpsilon g2 nm)
substWithEpsilon g _ = g
-}

-- Remove left recursion
--

{-


showRule (Terminal _) = "<terminal>"
showRule (NonTerminal nm g) = nm ++ ": " ++ (showRule' g)
showRule (Seq _ _) = "<seq>"
showRule (Alt _ _) = "<alt>"
showRule Epsilon = "<epsilon>"
        -}

cGrammar :: Grammar
cGrammar = nonTerminals translationUnit

type Doc = PP.Doc ()

showRule :: Rule -> Doc
showRule (Terminal txt) = PP.pretty $ "\"" ++ txt ++ "\""
showRule (NonTerminal nm) = PP.pretty nm
showRule (Seq gs) = PP.parens . PP.hsep . map showRule $ gs
showRule (Alt gs) = PP.parens . PP.sep . PP.punctuate (PP.pretty " |") . map showRule $ gs
showRule Epsilon = PP.pretty "epsilon"

main = forM_ (M.toList cGrammar) $ \(nm, g) -> do
    PP.putDocW 120 $ PP.pretty nm PP.<> PP.pretty ":" PP.<> PP.line PP.<> (PP.indent 4 $ showRule g)
    putStrLn ""
    putStrLn ""

