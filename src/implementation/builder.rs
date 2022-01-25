use std::{collections::HashMap, fmt::Display};

use rustdoc_types::{
    Crate, FnDecl, GenericArg, GenericArgs, Generics, Id, Impl, Item, ItemEnum, Type, GenericParamDef, GenericParamDefKind,
};

use super::item_utils;

/// Internal helper to keep track of state while analyzing the JSON
#[allow(clippy::module_name_repetitions)]
pub struct PublicItemBuilder<'a> {
    /// Maps an item ID to the container that contains it. Note that the
    /// container itself also is an item. E.g. an enum variant item is contained
    /// in an enum item.
    container_for_item: HashMap<&'a Id, &'a Item>,
}

impl<'a> PublicItemBuilder<'a> {
    pub fn new(crate_: &'a Crate) -> PublicItemBuilder<'a> {
        Self {
            container_for_item: item_utils::build_container_for_item_map(crate_),
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
        format!("pub {} ", item_utils::type_string_for_item(item))
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0.inner {
            _ => Ok(()),
            ItemEnum::Module(_) => Ok(()),
            ItemEnum::ExternCrate { name, rename } => todo!(),
            ItemEnum::Import(_) => todo!(),
            ItemEnum::Union(_) => todo!(),
            ItemEnum::Struct(s) => todo!(),
            ItemEnum::StructField(type_) => write!(f, ": {}", D(type_)),
            ItemEnum::Enum(e) => write!(f, ": {}", D(e)),
            ItemEnum::Variant(_) => todo!(),
            ItemEnum::Function(fn_) => write!(f, "{}", FnDeclaration(&fn_.decl)),
            ItemEnum::Trait(_) => todo!(),
            ItemEnum::TraitAlias(_) => todo!(),
            ItemEnum::Method(m) => write!(f, "{}", FnDeclaration(&m.decl)),
            ItemEnum::Impl(_) => todo!(),
            ItemEnum::Typedef(_) => todo!(),
            ItemEnum::OpaqueTy(_) => todo!(),
            ItemEnum::Constant(_) => todo!(),
            ItemEnum::Static(_) => todo!(),
            ItemEnum::ForeignType => todo!(),
            ItemEnum::Macro(_) | ItemEnum::ProcMacro(_) => write!(f, "{}", "!"),
            ItemEnum::PrimitiveType(_) => todo!(),
            ItemEnum::AssocConst { type_, default } => todo!(),
            ItemEnum::AssocType { bounds, default } => todo!(),
        }
    }
}

impl Display for D<&Generics> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.0.params.is_empty() {
            write!(f, "<{}>", Joiner(&self.0.params, ", ", |f| D(f)))?;
        }
        if !self.0.params.is_empty() {
            write!(f, " where {}", Joiner(&self.0.where_predicates, ", ", |f| D(f)))?;
        }

        Ok(())
    }
}

impl Display for D<&GenericParamDef> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.0.name, D(self.0.kind))
    }
}

impl Display for D<&GenericParamDefKind> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            
        }
        write!(f, "{}{}", self.0.))
    }
}

struct Joiner<'a, T, D: Display>(&'a Vec<T>, &'static str, fn(&'a T) -> D);
impl<'a, T, D: Display> Display for Joiner<'a, T, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            GenericArg::Lifetime(l) => write!(f, "{}", l),
            GenericArg::Type(t) => write!(f, "{}", D(t)),
            GenericArg::Const(_) => todo!(),
            GenericArg::Infer => todo!(),
        }
    }
}

impl Display for D<&Type> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.0 { "mut " } else { "" })
    }
}

struct Lifetime<'a>(&'a Option<String>);
impl Display for Lifetime<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(lifetime) = self.0 {
            write!(f, "'{} ", lifetime)
        } else {
            Ok(())
        }
    }
}
