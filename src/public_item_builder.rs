
use std::fmt::Result;
use std::fmt::Display;

struct ItemPath<'a>(&'a [Item]);
struct ItemInPath<'a>(&'a Item, ItemPath<'a>);

impl Display for ItemInPath<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let type_str = type_string_for_item(self.0);
        let path_str = self.1;
        let _s = [String::from("1")].join(", ");

        write!(f, "pub {type_str} {path_str}")
    }
}

impl Display for D<&Item> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}",         match self.0.inner {
            ItemEnum::Module(_) => todo!(),
            ItemEnum::ExternCrate { name, rename } => todo!(),
            ItemEnum::Import(_) => todo!(),
            ItemEnum::Union(_) => todo!(),
            ItemEnum::Struct(_) => todo!(),
            ItemEnum::StructField(_) => todo!(),
            ItemEnum::Enum(_) => todo!(),
            ItemEnum::Variant(_) => todo!(),
            ItemEnum::Function(_) => todo!(),
            ItemEnum::Trait(_) => todo!(),
            ItemEnum::TraitAlias(_) => todo!(),
            ItemEnum::Method(_) => todo!(),
            ItemEnum::Impl(_) => todo!(),
            ItemEnum::Typedef(_) => todo!(),
            ItemEnum::OpaqueTy(_) => todo!(),
            ItemEnum::Constant(_) => todo!(),
            ItemEnum::Static(_) => todo!(),
            ItemEnum::ForeignType => todo!(),
            ItemEnum::Macro(_) => todo!(),
            ItemEnum::ProcMacro(_) => todo!(),
            ItemEnum::PrimitiveType(_) => todo!(),
            ItemEnum::AssocConst { type_, default } => todo!(),
            ItemEnum::AssocType { bounds, default } => todo!(),
        }
)
    }
}

impl Display for ItemPath<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        todo!()
    }
}

fn type_string_for_item(item: &Item) -> &str {
    match &item.inner {
        ItemEnum::Module(_) => "mod",
        ItemEnum::ExternCrate { .. } => "extern crate",
        ItemEnum::Import(_) => "use",
        ItemEnum::Union(_) => "union",
        ItemEnum::Struct(_) => "struct",
        ItemEnum::StructField(_) => "struct field",
        ItemEnum::Enum(_) => "enum",
        ItemEnum::Variant(_) => "enum variant",
        ItemEnum::Function(_) | ItemEnum::Method(_) => "fn",
        ItemEnum::Trait(_) => "trait",
        ItemEnum::TraitAlias(_) => "trait alias",
        ItemEnum::Impl(_) => "impl",
        ItemEnum::Typedef(_) | ItemEnum::AssocType { .. } => "type",
        ItemEnum::OpaqueTy(_) => "opaque ty",
        ItemEnum::Constant(_) | ItemEnum::AssocConst { .. } => "const",
        ItemEnum::Static(_) => "static",
        ItemEnum::ForeignType => "foreign type",
        ItemEnum::Macro(_) => "macro",
        ItemEnum::ProcMacro(_) => "proc macro",
        ItemEnum::PrimitiveType(name) => name,
    }
}


use std::{collections::HashMap, fmt::Formatter};

use rustdoc_types::{
    Crate, FnDecl, GenericArg, GenericArgs, GenericBound, GenericParamDef, GenericParamDefKind,
    Generics, Id, Impl, Item, ItemEnum, Type, Variant, WherePredicate,
};

pub fn recursively_collect_child_items_from(crate_: Crate, root_item: &Item) -> Vec<&Item> {
    let result = vec![];

    let mut items_left_to_process = vec![root_item];

    while let Some(item) = items_left_to_process.pop() {
        let f = items_in_container(item).iter().flatten().map(|id| crate_.index.get(id));
        if let Some(items) = .iter().map(|id|crate_.index.get(id) {
            items_left_to_process.extend(items);

        }
    }
    result.push(root_item);


                                                                                                                                                                                                                                                                                                                                                                                                                                                       
    result
}

/// Internal helper to keep track of state while analyzing the JSON
pub struct PublicItemBuilder<'a> {
    /// Maps an item ID to the container that contains it. Note that the
    /// container itself also is an item. E.g. an enum variant item is contained
    /// in an enum item.
    container_for_item: HashMap<&'a Id, &'a Item>,
}

impl<'a> PublicItemBuilder<'a> {
    pub fn new(crate_: &'a Crate) -> PublicItemBuilder<'a> {
        Self {
            container_for_item: build_container_for_item_map(crate_),
        }
    }

    pub fn build_from_item(&self, item: &Item) -> String {
        let path = self
            .path_for_item(item)
            .iter()
            .map(|i| get_effective_name(i))
            .collect::<Vec<_>>();

        let mut result = format!(
            "{}{}{}",
            Self::prefix_for_item(item),
            path.join("::"),
            ItemSuffix(item),
        );

        // Inform users about buggy enum variant tuple struct fields if applicable
        if path.len() == 1 && matches!(item.inner, ItemEnum::StructField(_)) {
            result += " (path missing due to https://github.com/rust-lang/rust/issues/92945)";
        }

        result
    }

    fn container_for_item(&self, item: &Item) -> Option<&Item> {
        let effective_item_id = get_effective_id(item);
        self.container_for_item.get(effective_item_id).copied()
    }

    fn prefix_for_item(item: &Item) -> String {
        format!("pub {} ", type_string_for_item(item))
    }

    fn path_for_item(&'a self, item: &'a Item) -> Vec<&'a Item> {
        let mut path = vec![];
        path.insert(0, item);

        let mut current_item = item;
        while let Some(container) = self.container_for_item(current_item) {
            path.insert(0, container);
            current_item = container;
        }

        path
    }
}

fn get_effective_id(item: &Item) -> &Id {
    match &item.inner {
        ItemEnum::Impl(Impl {
            for_: Type::ResolvedPath { id, .. },
            ..
        }) => id,
        _ => &item.id,
    }
}

/// Some items do not use item.name. Handle that.
fn get_effective_name(item: &Item) -> &str {
    match &item.inner {
        // An import uses its own name (which can be different from the imported name)
        ItemEnum::Import(i) => &i.name,

        // An impl do not have a name. Instead the impl is _for_ something, like
        // a struct. In that case we want the name of the struct (for example).
        ItemEnum::Impl(
            Impl {
                for_: Type::ResolvedPath { name, .. },
                ..
            },
            ..,
        ) => name.as_ref(),

        _ => item.name.as_deref().unwrap_or("<<no_name>>"),
    }
}

struct FnDeclaration<'a>(&'a FnDecl);
impl Display for FnDeclaration<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        write!(
            f,
            "({})",
            self.0
                .inputs
                .iter()
                .map(|i| format!("{}: {}", i.0, D(&i.1)))
                .collect::<Vec<_>>()
                .join(", ")
        )?;
        if let Some(output) = &self.0.output {
            write!(f, " -> {}", D(output))?;
        }

        Ok(())
    }
}

struct ItemSuffix<'a>(&'a Item);
impl Display for ItemSuffix<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match &self.0.inner {
            ItemEnum::Module(_) | ItemEnum::Import(_)=> Ok(()),
            ItemEnum::ExternCrate { name, rename } => todo!(),
            ItemEnum::Union(_) => todo!(),
            ItemEnum::Struct(s) => write!(f, "{}", D(&s.generics)),
            ItemEnum::StructField(type_) => write!(f, ": {}", D(type_)),
            ItemEnum::Enum(e) => write!(f, "{}", D(&e.generics)),
            ItemEnum::Variant(v) => write!(f, "{}", D(v)),
            ItemEnum::Function(fn_) => write!(f, "{}", FnDeclaration(&fn_.decl)),
            ItemEnum::Trait(_) => todo!(),
            ItemEnum::TraitAlias(_) => todo!(),
            ItemEnum::Method(m) => write!(f, "{}", FnDeclaration(&m.decl)),
            ItemEnum::Impl(_) => todo!(),
            ItemEnum::Typedef(t) => write!(f, "{}", D(&t.generics)),
            ItemEnum::OpaqueTy(_) => todo!(),
            ItemEnum::Constant(c) => write!(f, ": {}", D(&c.type_)),
            ItemEnum::Static(_) => todo!(),
            ItemEnum::ForeignType => todo!(),
            ItemEnum::Macro(_) | ItemEnum::ProcMacro(_) => write!(f, "{}", "!"),
            ItemEnum::PrimitiveType(_) => todo!(),
            ItemEnum::AssocConst { type_, default } => write!(f, ": {}", D(type_)),
            ItemEnum::AssocType { bounds, default } => todo!(),
        }
    }
}

impl Display for D<&Generics> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        if !self.0.params.is_empty() {
            write!(f, "<{}>", Joiner(&self.0.params, ", ", |f| D(f)))?;
        }
        if !self.0.where_predicates.is_empty() {
            write!(
                f,
                " where {}",
                Joiner(&self.0.where_predicates, ", ", |f| D(f))
            )?;
        }

        Ok(())
    }
}

impl Display for D<&Variant> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self.0 {
            Variant::Plain => Ok(()),
            Variant::Tuple(types) => write!(f, "({})", Joiner(&types, ",", |f| D(f))),
            Variant::Struct(_) => Ok(()), // Each struct field is printed individually
        }
    }
}

// macro_rules! def {
//     ($t:ty, $bl:block) => {
//         impl Display for D<&$t> {
//             fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
//                 $bl
//         }
//     };
// }

// def!(GenericParamDef, {
//     write!(f, "{}{}", self.0.name, D(&self.0.kind))
// });

impl Display for D<&GenericParamDef> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        write!(f, "{}{}", self.0.name, D(&self.0.kind))
    }
}

impl Display for D<&WherePredicate> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        todo!()
    }
}

impl Display for D<&GenericParamDefKind> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self.0 {
            GenericParamDefKind::Lifetime { outlives } => {
                if !outlives.is_empty() {
                    write!(f, ": {}", outlives.join(", "))?;
                }
            }
            GenericParamDefKind::Type { bounds, default } => {
                if !bounds.is_empty() {
                    write!(f, ": {}", Joiner(bounds, ", ", |f| D(f)))?;
                }
            }
            GenericParamDefKind::Const { ty, default } => todo!(),
        }

        Ok(())
    }
}

impl Display for D<&GenericBound> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        todo!()
    }
}

struct Joiner<'a, T, D: Display>(&'a Vec<T>, &'static str, fn(&'a T) -> D);
impl<'a, T, D: Display> Display for Joiner<'a, T, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        write!(
            f,
            "{}",
            self.0
                .iter()
                .map(|t| format!("{}", self.2(t)))
                .collect::<Vec<_>>()
                .join(self.1)
        )
    }
}

impl Display for D<&GenericArgs> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match self.0 {
            GenericArgs::AngleBracketed { args, bindings } => {
                if !args.is_empty() {
                    write!(f, "<{}>", Joiner(args, ", ", |t| D(t)))?;
                }
            }
            GenericArgs::Parenthesized { inputs, output } => {
                if !inputs.is_empty() {
                    write!(f, "Fn<{}>", Joiner(inputs, ", ", |t| D(t)))?;
                }
            }
        }

        Ok(())
    }
}

struct D<T>(T);

impl Display for D<&GenericArg> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match &self.0 {
            GenericArg::Lifetime(l) => write!(f, "{}", l),
            GenericArg::Type(t) => write!(f, "{}", D(t)),
            GenericArg::Const(_) => todo!(),
            GenericArg::Infer => todo!(),
        }
    }
}

impl Display for D<&Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        match &self.0 {
            Type::ResolvedPath {
                name,
                args,
                param_names,
                ..
            } => {
                write!(f, "{}", name)?;
                if let Some(args) = args {
                    write!(f, "{}", D(args.as_ref()))?;
                }

                Ok(())
            }
            Type::Generic(s) => write!(f, "{}", s),
            Type::Primitive(p) => write!(f, "{}", p),
            Type::FunctionPointer(_) => todo!(),
            Type::Tuple(types_) => {
                write!(f, "({})", Joiner(types_, ",", |t| D(t)))
            }
            Type::Slice(t) => write!(f, "[{}]", D(t.as_ref())),
            Type::Array { type_, len } => todo!(), //write!("{}"),
            Type::ImplTrait(_) => todo!(),
            Type::Infer => todo!(),
            Type::RawPointer { mutable, type_ } => todo!(),
            Type::BorrowedRef {
                lifetime,
                mutable,
                type_,
            } => {
                write!(
                    f,
                    "&{}{}{}",
                    Lifetime(lifetime),
                    Mutable(*mutable),
                    D(type_.as_ref()),
                )
            }
            Type::QualifiedPath {
                name,
                self_type,
                trait_,
            } => write!(
                f,
                "<{} as {}>::{}",
                D(self_type.as_ref()),
                D(trait_.as_ref()),
                name
            ),
        }
    }
}

struct Mutable(bool);
impl Display for Mutable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        write!(f, "{}", if self.0 { "mut " } else { "" })
    }
}

struct Lifetime<'a>(&'a Option<String>);
impl Display for Lifetime<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result {
        if let Some(lifetime) = self.0 {
            write!(f, "'{} ", lifetime)
        } else {
            Ok(())
        }
    }
}

/// Map up what items are contained in what items. We can't limit this to
/// just our crate (the root crate) since some traits (e.g. Clone) are
/// defined outside of the root crate.
fn build_container_for_item_map(crate_: &Crate) -> HashMap<&Id, &Item> {
    let mut container_for_item = HashMap::new();

    for container in crate_.index.values() {
        if let Some(items) = items_in_container(container) {
            for item in items {
                container_for_item.insert(item, container);
            }
        }
    }

    container_for_item
}

