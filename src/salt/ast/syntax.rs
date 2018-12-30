use core::hash::Hash;
use core::hash::Hasher;
use langtools_common::langtools::common::position::BasicPosition;
use langtools_common::langtools::common::symbol::Symbol;
use salt_syntax_common::salt::common::syntax::AbstractionKind;
use salt_syntax_common::salt::common::syntax::Fieldname;
use salt_syntax_common::salt::common::syntax::Fixity;
use salt_syntax_common::salt::common::syntax::Literal;
use salt_syntax_common::salt::common::syntax::Prec;
use salt_syntax_common::salt::common::syntax::ScopeKind;
use salt_syntax_common::salt::common::syntax::TruthKind;
use salt_syntax_common::salt::common::syntax::Visibility;
use std::cmp::Ordering;

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct AST<'a> {
    /// The component declaration, if it exists.
    pub component: Option<Component<'a>>,
    /// Use statements.
    pub uses: Vec<Use<'a>>,
    /// The top-level scope.
    pub scope: Vec<Element<'a>>
}

/// A component declaration.  This gives the name of the component.
#[derive(Clone, Debug, Eq, Ord)]
pub struct Component<'a> {
    /// The component name.
    pub name: Vec<Symbol<'a>>,
    /// The position in source at which this occurs.
    pub pos: &'a BasicPosition<'a>
}

impl<'a> PartialEq for Component<'a> {
    fn eq(&self, other: &Component<'a>) -> bool {
        self.name == other.name
    }
}

impl<'a> PartialOrd for Component<'a> {
    fn partial_cmp(&self, other: &Component<'a>) -> Option<Ordering> {
        Some(self.name.cmp(&other.name))
    }
}

impl<'a> Hash for Component<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

/// Use directives.  This imports other components for use.
#[derive(Clone, Debug, Eq, Ord)]
pub struct Use<'a> {
    /// The component name.
    pub name: Vec<Symbol<'a>>,
    /// The position in source at which this occurs.
    pub pos: &'a BasicPosition<'a>
}

impl<'a> PartialEq for Use<'a> {
    fn eq(&self, other: &Use<'a>) -> bool {
        self.name == other.name
    }
}

impl<'a> PartialOrd for Use<'a> {
    fn partial_cmp(&self, other: &Use<'a>) -> Option<Ordering> {
        Some(self.name.cmp(&other.name))
    }
}

impl<'a> Hash for Use<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

#[derive(Clone, Debug, Eq, Ord)]
pub struct Group<'a> {
    pub visibility: Visibility,
    pub elements: Vec<Element<'a>>,
    /// The position in source at which this occurs.
    pub pos: &'a BasicPosition<'a>
}

impl<'a> PartialEq for Group<'a> {
    fn eq(&self, other: &Group<'a>) -> bool {
        self.visibility == other.visibility && self.elements == self.elements
    }
}

impl<'a> PartialOrd for Group<'a> {
    fn partial_cmp(&self, other: &Group<'a>) -> Option<Ordering> {
        match self.visibility.cmp(&other.visibility) {
            Ordering::Equal => self.elements.partial_cmp(&other.elements),
            out => Some(out)
        }
    }
}

impl<'a> Hash for Group<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.visibility.hash(state);
        self.elements.hash(state)
    }
}

/// Contenxt of a definition.  This is used wherever we can see a
/// scope budy or an expression referencing another definition.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum Content<'a> {
    /// An actual scope body.
    Body {
        scope: Scope<'a>
    },
    /// An expression referencing another definition.
    Value {
        exp: Exp<'a>
    },
    /// No body.
    None
}

type Scope<'a> = Vec<Group<'a>>;

/// Parameters to a scope fragment.
#[derive(Clone, Debug, Eq, Ord)]
pub struct Params<'a> {
    /// The definitions for the parameters.
    pub defs: Vec<Element<'a>>,
    /// The position in source at which this occurs.
    pub pos: &'a BasicPosition<'a>
}

impl<'a> PartialEq for Params<'a> {
    fn eq(&self, other: &Params<'a>) -> bool {
        self.defs == other.defs
    }
}

impl<'a> PartialOrd for Params<'a> {
    fn partial_cmp(&self, other: &Params<'a>) -> Option<Ordering> {
        Some(self.defs.cmp(&other.defs))
    }
}

impl<'a> Hash for Params<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.defs.hash(state)
    }
}

#[derive(Clone, Debug, Eq, Ord)]
pub enum Element<'a> {
    Fragment {
        kind: ScopeKind,
        name: Symbol<'a>,
        params: Option<Params<'a>>,
        supers: Vec<Exp<'a>>,
        content: Content<'a>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Def {
        pattern: Pattern<'a>,
        init: Option<Exp<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Truth {
        kind: TruthKind,
        name: Symbol<'a>,
        content: Exp<'a>,
        proof: Option<Exp<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Fun {
        name: Symbol<'a>,
        cases: Vec<Case<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Proof {
        name: Exp<'a>,
        body: Exp<'a>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Import {
        exp: Exp<'a>,
        names: Vec<Name<'a, Symbol<'a>>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Syntax {
        sym: Symbol<'a>,
        fixity: Fixity,
        precs: Vec<Prec<'a, Exp<'a>>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    }
}

impl<'a> PartialEq for Element<'a> {
    fn eq(&self, other: &Element<'a>) -> bool {
        match (self, other) {
            (Element::Fragment { kind: kind1, name: name1, params: params1,
                                 supers: supers1, content: content1, .. },
             Element::Fragment { kind: kind2, name: name2, params: params2,
                                 supers: supers2, content: content2, .. }) =>
                kind1 == kind2 && name1 == name2 && params1 == params2 &&
                supers1 == supers2 && content1 == content2,
            (Element::Def { pattern: pattern1, init: init1, .. },
             Element::Def { pattern: pattern2, init: init2, .. }) =>
                pattern1 == pattern2 && init1 == init2,
            (Element::Truth { kind: kind1, name: name1, content: content1,
                              proof: proof1, .. },
             Element::Truth { kind: kind2, name: name2, content: content2,
                              proof: proof2, .. }) =>
                kind1 == kind2 && name1 == name2 &&
                content1 == content2 && proof1 == proof2,
            (Element::Fun { name: name1, cases: cases1, .. },
             Element::Fun { name: name2, cases: cases2, .. }) =>
                name1 == name2 && cases1 == cases2,
            (Element::Proof { name: name1, body: body1, .. },
             Element::Proof { name: name2, body: body2, .. }) =>
                name1 == name2 && body1 == body2,
            (Element::Import { exp: exp1, names: names1, .. },
             Element::Import { exp: exp2, names: names2, .. }) =>
                exp1 == exp2 && names1 == names2,
            (Element::Syntax { sym: sym1, fixity: fixity1,
                               precs: precs1, .. },
             Element::Syntax { sym: sym2, fixity: fixity2,
                               precs: precs2, .. }) =>
                sym1 == sym2 && fixity1 == fixity2 && precs1 == precs2,
            _ => false
        }
    }
}

impl<'a> PartialOrd for Element<'a> {
    fn partial_cmp(&self, other: &Element<'a>) -> Option<Ordering> {
        match (self, other) {
            (Element::Fragment { kind: kind1, name: name1, params: params1,
                                 supers: supers1, content: content1, .. },
             Element::Fragment { kind: kind2, name: name2, params: params2,
                                 supers: supers2, content: content2, .. }) =>
                match kind1.cmp(kind2).then(name1.cmp(name2)) {
                    Ordering::Equal =>
                        match params1.partial_cmp(params2) {
                            Some(Ordering::Equal) =>
                                match supers1.partial_cmp(supers2) {
                                    Some(Ordering::Equal) =>
                                        content1.partial_cmp(content2),
                                    out => out
                                },
                            out => out
                        },
                    out => Some(out)
                },
            (_, Element::Fragment { .. }) => Some(Ordering::Less),
            (Element::Fragment { .. }, _) => Some(Ordering::Greater),
            (Element::Def { pattern: pattern1, init: init1, .. },
             Element::Def { pattern: pattern2, init: init2, .. }) =>
                match pattern1.partial_cmp(pattern2) {
                    Some(Ordering::Equal) => init1.partial_cmp(init2),
                    out => out
                }
            (_, Element::Def { .. }) => Some(Ordering::Less),
            (Element::Def { .. }, _) => Some(Ordering::Greater),
            (Element::Truth { kind: kind1, name: name1, content: content1,
                              proof: proof1, .. },
             Element::Truth { kind: kind2, name: name2, content: content2,
                              proof: proof2, .. }) =>
                match kind1.cmp(kind2).then(name1.cmp(name2)) {
                    Ordering::Equal => match content1.partial_cmp(content2) {
                        Some(Ordering::Equal) => proof1.partial_cmp(proof2),
                        out => out
                    },
                    out => Some(out)
                },
            (_, Element::Truth { .. }) => Some(Ordering::Less),
            (Element::Truth { .. }, _) => Some(Ordering::Greater),
            (Element::Fun { name: name1, cases: cases1, .. },
             Element::Fun { name: name2, cases: cases2, .. }) =>
                match name1.cmp(name2) {
                    Ordering::Equal => cases1.partial_cmp(cases2),
                    out => Some(out)
                },
            (_, Element::Fun { .. }) => Some(Ordering::Less),
            (Element::Fun { .. }, _) => Some(Ordering::Greater),
            (Element::Proof { name: name1, body: body1, .. },
             Element::Proof { name: name2, body: body2, .. }) =>
                match name1.cmp(name2) {
                    Ordering::Equal => body1.partial_cmp(body2),
                    out => Some(out)
                },
            (_, Element::Proof { .. }) => Some(Ordering::Less),
            (Element::Proof { .. }, _) => Some(Ordering::Greater),
            (Element::Import { exp: exp1, names: names1, .. },
             Element::Import { exp: exp2, names: names2, .. }) =>
                match exp1.cmp(exp2) {
                    Ordering::Equal => names1.partial_cmp(names2),
                    out => Some(out)
                },
            (_, Element::Import { .. }) => Some(Ordering::Less),
            (Element::Import { .. }, _) => Some(Ordering::Greater),
            (Element::Syntax { sym: sym1, fixity: fixity1,
                               precs: precs1, .. },
             Element::Syntax { sym: sym2, fixity: fixity2,
                               precs: precs2, .. }) =>
                match sym1.cmp(sym2).then(fixity1.cmp(fixity2)) {
                    Ordering::Equal => precs1.partial_cmp(precs2),
                    out => Some(out)
                }
        }
    }
}

impl<'a> Hash for Element<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Element::Fragment { kind, name, params, supers, content , .. } => {
                state.write_u8(0);
                kind.hash(state);
                name.hash(state);
                params.hash(state);
                supers.hash(state);
                content.hash(state);
            },
            Element::Def { pattern, init, .. } => {
                state.write_u8(1);
                pattern.hash(state);
                init.hash(state);
            },
            Element::Truth { kind, name, content, proof, .. } => {
                state.write_u8(2);
                kind.hash(state);
                name.hash(state);
                content.hash(state);
                proof.hash(state);
            },
            Element::Fun { name, cases, .. } => {
                state.write_u8(3);
                name.hash(state);
                cases.hash(state);
            },
            Element::Proof { name, body, .. } => {
                state.write_u8(4);
                name.hash(state);
                body.hash(state);
            },
            Element::Import { exp, names, .. } => {
                state.write_u8(5);
                exp.hash(state);
                names.hash(state);
            },
            Element::Syntax { sym, fixity, precs, .. } => {
                state.write_u8(6);
                sym.hash(state);
                fixity.hash(state);
                precs.hash(state);
            },
        }
    }
}

/// Compound expression elements.  These are either ordinary
/// expressions or definitions.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum Compound<'a> {
    /// An ordinary expression.
    Exp {
        val: Exp<'a>
    },
    /// A definition.
    Element {
        val: Element<'a>
    }
}

#[derive(Clone, Debug, Eq, Ord)]
pub enum Pattern<'a> {
    Option {
        pats: Vec<Pattern<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Deconstruct {
        name: Symbol<'a>,
        pat: Box<Pattern<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Split {
        fields: Vec<Entry<'a>>,
        strict: bool,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Typed {
        pat: Box<Pattern<'a>>,
        ty: Box<Exp<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    As {
        name: Symbol<'a>,
        pat: Box<Pattern<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Name {
        name: Symbol<'a>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Exact {
        literal: Literal<'a>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    }
}

impl<'a> PartialEq for Pattern<'a> {
    fn eq(&self, other: &Pattern<'a>) -> bool {
        match (self, other) {
            (Pattern::Option { pats: pat1, .. },
             Pattern::Option { pats: pat2, .. }) => pat1 == pat2,
            (Pattern::Deconstruct { name: name1, pat: pat1, .. },
             Pattern::Deconstruct { name: name2, pat: pat2, .. }) =>
                name1 == name2 && pat1 == pat2,
            (Pattern::Split { fields: fields1, strict: strict1, .. },
             Pattern::Split { fields: fields2, strict: strict2, .. }) =>
                strict1 == strict2 && fields1 == fields2,
            (Pattern::Typed { pat: pat1, ty: ty1, .. },
             Pattern::Typed { pat: pat2, ty: ty2, .. }) =>
                pat1 == pat2 && ty1 == ty2,
            (Pattern::As { name: name1, pat: pat1, .. },
             Pattern::As { name: name2, pat: pat2, .. }) =>
                name1 == name2 && pat1 == pat2,
            (Pattern::Name { name: name1, .. },
             Pattern::Name { name: name2, .. }) => name1 == name2,
            (Pattern::Exact { literal: literal1, .. },
             Pattern::Exact { literal: literal2, .. }) => literal1 == literal2,
            _ => false
        }
    }
}

impl<'a> PartialOrd for Pattern<'a> {
    fn partial_cmp(&self, other: &Pattern<'a>) -> Option<Ordering> {
        match (self, other) {
            (Pattern::Option { pats: pat1, .. },
             Pattern::Option { pats: pat2, .. }) => pat1.partial_cmp(pat2),
            (_, Pattern::Option { .. }) => Some(Ordering::Less),
            (Pattern::Option { .. }, _) => Some(Ordering::Greater),
            (Pattern::Deconstruct { name: name1, pat: pat1, .. },
             Pattern::Deconstruct { name: name2, pat: pat2, .. }) =>
                match name1.cmp(name2) {
                    Ordering::Equal => pat1.partial_cmp(pat2),
                    out => Some(out)
                },
            (_, Pattern::Deconstruct { .. }) => Some(Ordering::Less),
            (Pattern::Deconstruct { .. }, _) => Some(Ordering::Greater),
            (Pattern::Split { fields: fields1, strict: strict1, .. },
             Pattern::Split { fields: fields2, strict: strict2, .. }) =>
                match strict1.cmp(strict2) {
                    Ordering::Equal => fields1.partial_cmp(fields2),
                    out => Some(out)
                },
            (_, Pattern::Split { .. }) => Some(Ordering::Less),
            (Pattern::Split { .. }, _) => Some(Ordering::Greater),
            (Pattern::Typed { pat: pat1, ty: ty1, .. },
             Pattern::Typed { pat: pat2, ty: ty2, .. }) =>
                match pat1.partial_cmp(pat2) {
                    Some(Ordering::Equal) => ty1.partial_cmp(ty2),
                    out => out
                },
            (_, Pattern::Typed { .. }) => Some(Ordering::Less),
            (Pattern::Typed { .. }, _) => Some(Ordering::Greater),
            (Pattern::As { name: name1, pat: pat1, .. },
             Pattern::As { name: name2, pat: pat2, .. }) =>
                match name1.cmp(name2) {
                    Ordering::Equal => pat1.partial_cmp(pat2),
                    out => Some(out)
                },
            (_, Pattern::As { .. }) => Some(Ordering::Less),
            (Pattern::As { .. }, _) => Some(Ordering::Greater),
            (Pattern::Name { name: name1, .. },
             Pattern::Name { name: name2, .. }) => name1.partial_cmp(name2),
            (_, Pattern::Name { .. }) => Some(Ordering::Less),
            (Pattern::Name { .. }, _) => Some(Ordering::Greater),
            (Pattern::Exact { literal: literal1, .. },
             Pattern::Exact { literal: literal2, .. }) =>
                literal1.partial_cmp(literal2)
        }
    }
}

impl<'a> Hash for Pattern<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Pattern::Option { pats, .. } => {
                state.write_u8(0);
                pats.hash(state);
            },
            Pattern::Deconstruct { name, pat, .. } => {
                state.write_u8(1);
                name.hash(state);
                pat.hash(state);
            },
            Pattern::Split { fields, strict, .. } => {
                state.write_u8(2);
                fields.hash(state);
                strict.hash(state);
            },
            Pattern::Typed { pat, ty, .. } => {
                state.write_u8(3);
                pat.hash(state);
                ty.hash(state);
            },
            Pattern::As { name, pat, .. } => {
                state.write_u8(4);
                name.hash(state);
                pat.hash(state);
            },
            Pattern::Name { name, .. } => {
                state.write_u8(5);
                name.hash(state);
            },
            Pattern::Exact { literal, .. } => {
                state.write_u8(6);
                literal.hash(state);
            }
        }
    }
}

#[derive(Clone, Debug, Eq, Ord)]
pub enum Exp<'a> {
    Compound {
        body: Vec<Compound<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Abs {
        kind: AbstractionKind,
        cases: Vec<Case<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Match {
        val: Box<Exp<'a>>,
        cases: Vec<Case<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    /// Ascribe expression.  Fixes the type of a given expression.
    Ascribe {
        /// The expression whose type is being fixed.
        val: Box<Exp<'a>>,
        /// The type.
        ty: Box<Exp<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Seq {
        exps: Vec<Exp<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Record {
        fields: Vec<Field<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    RecordType {
        fields: Vec<Field<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Project {
        val: Box<Exp<'a>>,
        names: Vec<Name<'a, Fieldname<'a>>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Ref {
        name: Symbol<'a>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    With {
        val: Box<Exp<'a>>,
        args: Box<Exp<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Where {
        val: Box<Exp<'a>>,
        prop: Box<Exp<'a>>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Anon {
        kind: ScopeKind,
        supers: Vec<Exp<'a>>,
        params: Option<Params<'a>>,
        content: Scope<'a>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    /// A literal value.
    Literal {
        /// The literal.
        literal: &'a Literal<'a>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    }
}

impl<'a> PartialEq for Exp<'a> {
    fn eq(&self, other: &Exp<'a>) -> bool {
        match (self, other) {
            (Exp::Compound { body: body1, .. },
             Exp::Compound { body: body2, .. }) => body1 == body2,
            (Exp::Abs { kind: kind1, cases: cases1, .. },
             Exp::Abs { kind: kind2, cases: cases2, .. }) =>
                kind1 == kind2 && cases1 == cases2,
            (Exp::Match { val: val1, cases: cases1, .. },
             Exp::Match { val: val2, cases: cases2, .. }) =>
                val1 == val2 && cases1 == cases2,
            (Exp::Ascribe { val: val1, ty: ty1, .. },
             Exp::Ascribe { val: val2, ty: ty2, .. }) =>
                val1 == val2 && ty1 == ty2,
            (Exp::Seq { exps: exps1, .. },
             Exp::Seq { exps: exps2, .. }) => exps1 == exps2,
            (Exp::Record { fields: fields1, .. },
             Exp::Record { fields: fields2, .. }) => fields1 == fields2,
            (Exp::RecordType { fields: fields1, .. },
             Exp::RecordType { fields: fields2, .. }) => fields1 == fields2,
            (Exp::Project { val: val1, names: names1, .. },
             Exp::Project { val: val2, names: names2, .. }) =>
                val1 == val2 && names1 == names2,
            (Exp::Ref { name: name1, .. },
             Exp::Ref { name: name2, .. }) => name1 == name2,
            (Exp::With { val: val1, args: args1, .. },
             Exp::With { val: val2, args: args2, .. }) =>
                val1 == val2 && args1 == args2,
            (Exp::Where { val: val1, prop: prop1, .. },
             Exp::Where { val: val2, prop: prop2, .. }) =>
                val1 == val2 && prop1 == prop2,
            (Exp::Anon { kind: kind1, supers: supers1, params: params1,
                         content: content1, .. },
             Exp::Anon { kind: kind2, supers: supers2, params: params2,
                         content: content2, .. }) =>
                kind1 == kind2 && supers1 == supers2 &&
                params1 == params2 && content1 == content2,
            (Exp::Literal { literal: literal1, .. },
             Exp::Literal { literal: literal2, .. }) => literal1 == literal2,
            _ => false
        }
    }
}

impl<'a> PartialOrd for Exp<'a> {
    fn partial_cmp(&self, other: &Exp<'a>) -> Option<Ordering> {
        match (self, other) {
            (Exp::Compound { body: body1, .. },
             Exp::Compound { body: body2, .. }) =>
                body1.partial_cmp(body2),
            (_, Exp::Compound { .. }) => Some(Ordering::Less),
            (Exp::Compound { .. }, _) => Some(Ordering::Greater),
            (Exp::Abs { kind: kind1, cases: cases1, .. },
             Exp::Abs { kind: kind2, cases: cases2, .. }) =>
                match kind1.cmp(kind2) {
                    Ordering::Equal => cases1.partial_cmp(cases2),
                    out => Some(out)
                },
            (_, Exp::Abs { .. }) => Some(Ordering::Less),
            (Exp::Abs { .. }, _) => Some(Ordering::Greater),
            (Exp::Match { val: val1, cases: cases1, .. },
             Exp::Match { val: val2, cases: cases2, .. }) =>
                match val1.partial_cmp(val2) {
                    Some(Ordering::Equal) => cases1.partial_cmp(cases2),
                    out => out
                },
            (_, Exp::Match { .. }) => Some(Ordering::Less),
            (Exp::Match { .. }, _) => Some(Ordering::Greater),
            (Exp::Ascribe { val: val1, ty: ty1, .. },
             Exp::Ascribe { val: val2, ty: ty2, .. }) =>
                match val1.partial_cmp(val2) {
                    Some(Ordering::Equal) => ty1.partial_cmp(ty2),
                    out => out
                },
            (_, Exp::Ascribe { .. }) => Some(Ordering::Less),
            (Exp::Ascribe { .. }, _) => Some(Ordering::Greater),
            (Exp::Seq { exps: exps1, .. },
             Exp::Seq { exps: exps2, .. }) =>
                exps1.partial_cmp(exps2),
            (_, Exp::Seq { .. }) => Some(Ordering::Less),
            (Exp::Seq { .. }, _) => Some(Ordering::Greater),
            (Exp::Record { fields: fields1, .. },
             Exp::Record { fields: fields2, .. }) =>
                fields1.partial_cmp(fields2),
            (_, Exp::Record { .. }) => Some(Ordering::Less),
            (Exp::Record { .. }, _) => Some(Ordering::Greater),
            (Exp::RecordType { fields: fields1, .. },
             Exp::RecordType { fields: fields2, .. }) =>
                fields1.partial_cmp(fields2),
            (_, Exp::RecordType { .. }) => Some(Ordering::Less),
            (Exp::RecordType { .. }, _) => Some(Ordering::Greater),
            (Exp::Project { val: val1, names: names1, .. },
             Exp::Project { val: val2, names: names2, .. }) =>
                match val1.partial_cmp(val2) {
                    Some(Ordering::Equal) => names1.partial_cmp(names2),
                    out => out
                },
            (_, Exp::Project { .. }) => Some(Ordering::Less),
            (Exp::Project { .. }, _) => Some(Ordering::Greater),
            (Exp::Ref { name: name1, .. },
             Exp::Ref { name: name2, .. }) =>
                name1.partial_cmp(name2),
            (_, Exp::Ref { .. }) => Some(Ordering::Less),
            (Exp::Ref { .. }, _) => Some(Ordering::Greater),
            (Exp::With { val: val1, args: args1, .. },
             Exp::With { val: val2, args: args2, .. }) =>
                match val1.partial_cmp(val2) {
                    Some(Ordering::Equal) => args1.partial_cmp(args2),
                    out => out
                },
            (_, Exp::With { .. }) => Some(Ordering::Less),
            (Exp::With { .. }, _) => Some(Ordering::Greater),
            (Exp::Where { val: val1, prop: prop1, .. },
             Exp::Where { val: val2, prop: prop2, .. }) =>
                match val1.partial_cmp(val2) {
                    Some(Ordering::Equal) => prop1.partial_cmp(prop2),
                    out => out
                },
            (_, Exp::Where { .. }) => Some(Ordering::Less),
            (Exp::Where { .. }, _) => Some(Ordering::Greater),
            (Exp::Anon { kind: kind1, supers: supers1, params: params1,
                         content: content1, .. },
             Exp::Anon { kind: kind2, supers: supers2, params: params2,
                         content: content2, .. }) =>
                match kind1.cmp(kind2).then(supers1.cmp(supers2)) {
                    Ordering::Equal =>
                        match params1.partial_cmp(params2) {
                            Some(Ordering::Equal) =>
                                content1.partial_cmp(content2),
                            out => out
                        },
                    out => Some(out)
                },
            (_, Exp::Anon { .. }) => Some(Ordering::Less),
            (Exp::Anon { .. }, _) => Some(Ordering::Greater),
            (Exp::Literal { literal: literal1, .. },
             Exp::Literal { literal: literal2, .. }) =>
                literal1.partial_cmp(literal2)
        }
    }
}

impl<'a> Hash for Exp<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Exp::Compound { body, .. } => {
                state.write_u8(0);
                body.hash(state);
            },
            Exp::Abs { kind, cases, .. } => {
                state.write_u8(1);
                kind.hash(state);
                cases.hash(state);
            },
            Exp::Match { val, cases, .. } => {
                state.write_u8(2);
                val.hash(state);
                cases.hash(state);
            },
            Exp::Ascribe { val, ty, .. } => {
                state.write_u8(3);
                val.hash(state);
                ty.hash(state);
            },
            Exp::Seq { exps, .. } => {
                state.write_u8(4);
                exps.hash(state);
            },
            Exp::Record { fields, .. } => {
                state.write_u8(5);
                fields.hash(state);
            },
            Exp::RecordType { fields, .. } => {
                state.write_u8(6);
                fields.hash(state);
            },
            Exp::Project { val, names, .. } => {
                state.write_u8(7);
                val.hash(state);
                names.hash(state);
            },
            Exp::Ref { name, .. } => {
                state.write_u8(8);
                name.hash(state);
            },
            Exp::With { val, args, .. } => {
                state.write_u8(9);
                val.hash(state);
                args.hash(state);
            },
            Exp::Where { val, prop, .. } => {
                state.write_u8(10);
                val.hash(state);
                prop.hash(state);
            },
            Exp::Anon { kind, supers, params, content, .. } => {
                state.write_u8(11);
                kind.hash(state);
                supers.hash(state);
                params.hash(state);
                content.hash(state);
            },
            Exp::Literal { literal, .. } => {
                state.write_u8(12);
                literal.hash(state);
            }
        }
    }
}


#[derive(Clone, Debug, Eq, Ord)]
pub enum Entry<'a> {
    Named {
        name: Symbol<'a>,
        pattern: Pattern<'a>,
        /// The position in source at which this occurs.
        pos: &'a BasicPosition<'a>
    },
    Unnamed {
        pattern: Pattern<'a>
    }
}
impl<'a> PartialEq for Entry<'a> {
    fn eq(&self, other: &Entry<'a>) -> bool {
        match (self, other) {
            (Entry::Named { name: name1, pattern: pattern1, .. },
             Entry::Named { name: name2, pattern: pattern2, .. }) =>
                name1 == name2 && pattern1 == pattern2,
            (Entry::Unnamed { pattern: pattern1 },
             Entry::Unnamed { pattern: pattern2 }) => pattern1 == pattern2,
            _ => false
        }
    }
}

impl<'a> PartialOrd for Entry<'a> {
    fn partial_cmp(&self, other: &Entry<'a>) -> Option<Ordering> {
        match (self, other) {
            (Entry::Named { name: name1, pattern: pattern1, .. },
             Entry::Named { name: name2, pattern: pattern2, .. }) =>
                match name1.cmp(name2) {
                    Ordering::Equal => pattern1.partial_cmp(pattern2),
                    out => Some(out)
                },
            (_, Entry::Named { .. }) => Some(Ordering::Less),
            (Entry::Named { .. }, _) => Some(Ordering::Greater),
            (Entry::Unnamed { pattern: pattern1 },
             Entry::Unnamed { pattern: pattern2 }) =>
                pattern1.partial_cmp(pattern2)
        }
    }
}

impl<'a> Hash for Entry<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Entry::Named { name, pattern, .. } => {
                state.write_u8(0);
                name.hash(state);
                pattern.hash(state)
            },
            Entry::Unnamed { pattern } => {
                state.write_u8(1);
                pattern.hash(state)
            },
        }
    }
}

/// A field in a record.
#[derive(Clone, Debug, Eq, Ord)]
pub struct Field<'a> {
    /// The name being bound.
    pub name: Symbol<'a>,
    /// The value being assigned to the bound name.
    pub val: Exp<'a>,
    /// The position in source at which this occurs.
    pub pos: &'a BasicPosition<'a>
}

impl<'a> PartialEq for Field<'a> {
    fn eq(&self, other: &Field<'a>) -> bool {
        self.name == other.name && self.val == self.val
    }
}

impl<'a> PartialOrd for Field<'a> {
    fn partial_cmp(&self, other: &Field<'a>) -> Option<Ordering> {
        match self.name.cmp(&other.name) {
            Ordering::Equal => self.val.partial_cmp(&other.val),
            out => Some(out)
        }
    }
}

impl<'a> Hash for Field<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.val.hash(state)
    }
}

#[derive(Clone, Debug, Eq, Ord)]
pub struct Name<'a, S> {
    /// The name.
    pub name: S,
    /// The position in source at which this occurs.
    pub pos: &'a BasicPosition<'a>
}

impl<'a, S: PartialEq> PartialEq for Name<'a, S> {
    fn eq(&self, other: &Name<'a, S>) -> bool {
        self.name == other.name
    }
}

impl<'a, S: PartialOrd> PartialOrd for Name<'a, S> {
    fn partial_cmp(&self, other: &Name<'a, S>) -> Option<Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl<'a, S: Hash> Hash for Name<'a, S> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state)
    }
}

/// A field in a record.
#[derive(Clone, Debug, Eq, Ord)]
pub struct Case<'a> {
    /// The pattern to match for this case.
    pub pattern: Pattern<'a>,
    /// The expression to execute for this case.
    pub body: Exp<'a>,
    /// The position in source at which this occurs.
    pub pos: &'a BasicPosition<'a>
}

impl<'a> PartialEq for Case<'a> {
    fn eq(&self, other: &Case<'a>) -> bool {
        self.pattern == other.pattern && self.body == self.body
    }
}

impl<'a> PartialOrd for Case<'a> {
    fn partial_cmp(&self, other: &Case<'a>) -> Option<Ordering> {
        match self.pattern.cmp(&other.pattern) {
            Ordering::Equal => self.body.partial_cmp(&other.body),
            out => Some(out)
        }
    }
}

impl<'a> Hash for Case<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pattern.hash(state);
        self.body.hash(state)
    }
}
