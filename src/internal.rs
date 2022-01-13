use std::collections::{HashMap, HashSet};

use rustdoc_types::{
    Crate, GenericArg, GenericArgs, Generics, Id, Impl, Item, ItemEnum, Type, Visibility,
};
use serde_json::map::IntoIter;

use crate::Result;

/// Takes rustdoc JSON and returns a `HashSet` of `String`s where each `String`
/// is a public item of the crate, i.e. part of the crate's public API.
///
/// Use
/// ```bash
/// RUSTDOCFLAGS='-Z unstable-options --output-format json' cargo +nightly doc --lib --no-deps
/// ```
/// to generate rustdoc JSON. The rustdoc JSON format is documented here:
/// <https://rust-lang.github.io/rfcs/2963-rustdoc-json.html>.
///
/// # Errors
///
/// E.g. if the JSON is invalid.
pub fn from_rustdoc_json_str(rustdoc_json_str: &str) -> Result<HashSet<String>> {
    let rustdoc_json: Crate = serde_json::from_str(rustdoc_json_str)?;

    let helper = RustdocJsonHelper::new(&rustdoc_json);

    Ok(helper
        .public_items_in_root_crate()
        .map(|item| helper.full_item_name_with_type_prefix_and_suffix(item))
        .collect())
}

/// Internal helper to keep track of state while analyzing the JSON
struct RustdocJsonHelper<'a> {
    rustdoc_json: &'a Crate,

    /// Maps an item ID to the container that contains it. Note that the
    /// container itself also is an item. E.g. an enum variant is contained in
    /// an enum item.
    item_id_to_container: HashMap<&'a Id, &'a Item>,
}

impl<'a> RustdocJsonHelper<'a> {
    fn new(rustdoc_json: &'a Crate) -> RustdocJsonHelper<'a> {
        // Map up what items are contained in what items. We can't limit this to
        // just our crate (the root crate) since some traits (e.g. Clone) are
        // defined outside of the root crate.
        let mut item_id_to_container: HashMap<&Id, &Item> = HashMap::new();
        for item in rustdoc_json.index.values() {
            if let Some(contained_item_ids) = contained_items_in_item(item) {
                for contained_item_id in contained_item_ids {
                    item_id_to_container.insert(contained_item_id, item);
                }
            }
        }

        Self {
            rustdoc_json,
            item_id_to_container,
        }
    }

    fn public_items_in_root_crate(&self) -> impl Iterator<Item = &Item> {
        const ROOT_CRATE_ID: u32 = 0;

        self.rustdoc_json
            .index
            .values()
            .filter(|item| item.crate_id == ROOT_CRATE_ID && self.item_effectively_public(item))
    }

    /// Some items, notably enum variants in public enums, and associated
    /// functions in public traits, are public even though they have default
    /// visibility. This helper takes care of such cases.
    fn item_effectively_public(&self, item: &Item) -> bool {
        if let Some(container) = self.item_id_to_container.get(&item.id) {
            match &container.inner {
                // The item is implemented an associated method in a trait.
                // Since we know about the trait, it must be a public trait. So
                // the associated fn must also be effectively public.
                ItemEnum::Impl(Impl {
                    trait_: Some(Type::ResolvedPath { .. }),
                    ..
                })

                // The item is contained in an enum, so it is an enum variant.
                // If the enum itself is public, then so are its variants. Since
                // the enum would not be in the rustdoc JSON if it was not
                // public, we know this variant is public.
                | ItemEnum::Enum(_) => false,

                // The item is contained neither in an enum nor a trait. Such
                // items are only public if they actually are declared public.
                _ => item.visibility == Visibility::Public,
            }
        } else {
            // The item is not contained in some other item. So it is only
            // public if declared public.
            item.visibility == Visibility::Public
        }
    }

    /// Returns the name of an item, including the path from the crate root.
    fn full_item_name(&self, item: &Item) -> String {
        let mut s = String::new();
        let mut current_item = item;
        loop {
            current_item = if let Some(container) = self.container_for_item(current_item) {
                s = format!("::{}", get_effective_name(current_item)) + &s;
                container
            } else {
                s = get_effective_name(current_item).to_owned() + &s;
                break;
            }
        }
        s
    }

    fn full_item_name_with_type_prefix_and_suffix(&self, item: &Item) -> String {
        String::from("pub ")
            + self.item_type_prefix(item)
            + " "
            + &self.full_item_name(item)
            + &self.item_type_suffix(item)
    }

    fn item_type_prefix<'b>(&self, item: &'b Item) -> &'b str {
        match &item.inner {
            ItemEnum::Module(_) => "mod",
            ItemEnum::ExternCrate { .. } => todo!(),
            ItemEnum::Import(_) => "use",
            ItemEnum::Union(_) => "union",
            ItemEnum::Struct(_) => "struct",
            ItemEnum::StructField(_) => "struct", // For grouped sorting
            ItemEnum::Enum(_) => "enum",
            ItemEnum::Variant(_) => "enum", // For grouped sorting
            ItemEnum::Function(_) => "fn",
            ItemEnum::Trait(_) => "trait",
            ItemEnum::TraitAlias(_) => "trait alias",
            ItemEnum::Method(_) => "fnm",
            ItemEnum::Impl(_) => "impl",
            ItemEnum::Typedef(_) => "type",
            ItemEnum::OpaqueTy(_) => todo!(),
            ItemEnum::Constant(_) => "const",
            ItemEnum::Static(_) => "static",
            ItemEnum::ForeignType => todo!(),
            ItemEnum::Macro(_) => todo!(),
            ItemEnum::ProcMacro(_) => todo!(),
            ItemEnum::PrimitiveType(name) => &name,
            ItemEnum::AssocConst { .. } => "const",
            ItemEnum::AssocType { .. } => "type",
        }
    }

    fn item_type_suffix(&self, item: &Item) -> String {
        match &item.inner {
            ItemEnum::Union(u) => self.generics_to_string(&u.generics),
            ItemEnum::Struct(s) => self.generics_to_string(&s.generics),
            ItemEnum::StructField(ty) => format!(": {}", self.type_to_string(ty)),
            ItemEnum::Variant(_) => String::from(": ..."),
            ItemEnum::Function(f) => self.fn_decl_to_string(&f.decl),
            ItemEnum::Trait(t) => self.generics_to_string(&t.generics),
            ItemEnum::Method(m) => self.fn_decl_to_string(&m.decl),
            ItemEnum::Typedef(_) => String::from("= ..."),
            ItemEnum::Constant(_) => String::from("= ..."),
            ItemEnum::Static(_) => String::from("= ..."),
            ItemEnum::PrimitiveType(name) => name.clone(),
            _ => String::from(""),
        }
        .to_owned()
    }

    fn fn_decl_to_string(&self, decl: &rustdoc_types::FnDecl) -> String {
        let mut s = String::from("(");
        let a: Vec<String> = decl
            .inputs
            .iter()
            .map(|i| &i.1)
            .map(|t| self.type_to_string(&t))
            .collect();
        s.push_str(&a.join(", "));
        s.push_str(") -> ");
        s.push_str(&match &decl.output {
            Some(foo) => self.type_to_string(&foo),
            None => "".to_owned(),
        });
        s
    }

    fn type_to_string(&self, ty: &Type) -> String {
        match ty {
            Type::ResolvedPath {
                name,
                id,
                args,
                param_names,
            } => {
                let mut s = String::from(name);
                if let Some(g) = args {
                    s.push_str(&self.generics_arg_to_string(g));
                }

                s.push_str(
                    &param_names
                        .iter()
                        .map(|a| format!("{:?}", a))
                        .collect::<Vec<_>>()
                        .join("    ,   "),
                );
                s
            }
            Type::Generic(g) => format!("{}", g),
            Type::Primitive(p) => p.to_owned(),
            Type::FunctionPointer(_) => todo!(),
            Type::Tuple(types) => self.to_str_tuple(&types),
            Type::Slice(ty) => format!("[{}]", self.type_to_string(ty)),
            Type::Array { type_, len } => todo!(),
            Type::ImplTrait(_) => todo!(),
            Type::Infer => todo!(),
            Type::RawPointer { mutable, type_ } => todo!(),
            Type::BorrowedRef {
                lifetime,
                mutable,
                type_,
            } => {
                let mut s = String::from("&");
                if let Some(l) = lifetime {
                    s.push_str(l);
                    s.push_str(" ");
                }
                if *mutable {
                    s.push_str("mut ")
                }
                s.push_str(&self.type_to_string(type_));
                s
            }
            Type::QualifiedPath {
                name,
                self_type,
                trait_,
            } => todo!(),
        }
    }

    fn generics_to_string(&self, generics: &Generics) -> String {
        let mut s = String::new();
        let mut at_least_one_generic_param_added = false;

        for generic_param_def in &generics.params {
            if !at_least_one_generic_param_added {
                at_least_one_generic_param_added = true;
                s.push_str("<");
            } else {
                s.push_str(",");
            }
            s.push_str(&generic_to_string(generic_param_def));
        }

        if at_least_one_generic_param_added {
            s.push_str(">");
        }

        if generics.params.len() == 0 && generics.where_predicates.len() == 0 {
            s.push_str("");
        } else {
            if generics.where_predicates.len() > 0 {
                s.push_str(" where ...");
            }
        }

        s
    }

    fn container_for_item(&self, item: &Item) -> Option<&Item> {
        let effective_item_id = get_effective_id(item);
        self.item_id_to_container.get(effective_item_id).copied()
    }

    fn to_str_tuple(&self, types: &[Type]) -> String {
        let mut s = String::from("(");
        let type_strings: Vec<String> = types.iter().map(|i| self.type_to_string(i)).collect();
        type_strings.join(",");
        s.push_str(")");
        s
    }

    fn generics_arg_to_string(&self, generics: &GenericArgs) -> String {
        match generics {
            GenericArgs::AngleBracketed { args, bindings } => {
                let mut s = String::new();
                if args.len() > 0 {
                    s.push_str("<");
                    for a in args {
                        s.push_str(&self.generic_arg_to_string(a));
                    }
                    s.push_str(">");
                }
                s
            }
            GenericArgs::Parenthesized { inputs, output } => {
                format!("GenericArgs::Parenthesized")
            }
        }
    }

    fn generic_arg_to_string(&self, generic_arg: &GenericArg) -> String {
        match generic_arg {
            GenericArg::Lifetime(s) => s.clone(),
            GenericArg::Type(t) => self.type_to_string(t),
            GenericArg::Const(_) => todo!(),
            GenericArg::Infer => todo!(),
        }
    }
}

fn generic_to_string(generic_param_def: &rustdoc_types::GenericParamDef) -> String {
    format!("{}", generic_param_def.name)
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

/// Some items contain other items, which is relevant for analysis. Keep track
/// of such relationships.
fn contained_items_in_item(item: &Item) -> Option<&Vec<Id>> {
    match &item.inner {
        ItemEnum::Module(m) => Some(&m.items),
        ItemEnum::Union(u) => Some(&u.fields),
        ItemEnum::Struct(s) => Some(&s.fields),
        ItemEnum::Enum(e) => Some(&e.variants),
        ItemEnum::Trait(t) => Some(&t.items),
        ItemEnum::Impl(i) => Some(&i.items),
        _ => None,
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

fn print_if_present<T>(
    left: &str,
    v: impl std::iter::IntoIterator<Item = T>,
    sep: &str,
    right: &str,
) -> String
where
    T: ToString,
{
    let mut s = String::new();

    let mut i = v.into_iter().peekable();

    if let Some(_) = i.peek() {
        s.push_str(left);

        s.push_str(&i.map(|f| f.to_string2()).collect::<Vec<_>>().join(sep));

        s.push_str(right);
    }

    s
}

struct Display {}

impl std::fmt::Display for Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "hej")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_if_present() {
        let mut v = vec!["T", "U"];
        assert_eq!("<T, U>", print_if_present("<", &v, ", ", ">"));
        v.clear();
        assert_eq!("", print_if_present("<", &v, ", ", ">"));
    }

    // For tests
    impl ToString for &&str {
        fn to_string2(&self) -> String {
            self.to_string()
        }
    }
}

trait ToString {
    fn to_string2(&self) -> String;
}
