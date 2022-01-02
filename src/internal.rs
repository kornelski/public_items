use std::collections::{HashMap, HashSet};

use rustdoc_types::{Crate, Id, Item, ItemEnum, Type, Visibility};

use crate::Result;

const ROOT_CRATE_ID: u32 = 0;

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
    let analyzer = RustdocJsonAnalyzer::new(&rustdoc_json);

    let index = rustdoc_json.index;

    let mut public_items = vec![];

    // Now find all public items in the root crate.
    for item in index.values() {
        if item.crate_id != ROOT_CRATE_ID {
            continue;
        }

        let effectively_public = analyzer.item_is_effectively_public(item);
        if item.visibility == Visibility::Public || effectively_public {
            public_items.push(item);
        }
    }

    let mut res = vec![];
    for public_item in public_items {
        let mut s = String::new();
        analyzer.item_name_with_parents(public_item, &mut s);
        res.push(s);
    }

    Ok(res.into_iter().collect::<HashSet<_>>())
}

/// Internal helper to keep track of state while analyzing the JSON
struct RustdocJsonAnalyzer<'a> {
    rustdoc_json: &'a Crate,

    /// Maps an item ID to the container that contains it. Note that the
    /// container itself also is an item.
    item_id_to_container: HashMap<&'a Id, &'a Item>,
}

impl<'a> RustdocJsonAnalyzer<'a> {
    fn new(rustdoc_json: &'a Crate) -> RustdocJsonAnalyzer<'a> {
        // Map up what items are contained in what items. We can't limit this to
        // just our crate (the root crate) since some traits (e.g. Clone) are
        // defined outside of our crate.
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

    /// Some items, notably enum variants in public enums, and associated
    /// functions in public traits, are public even though they have default
    /// visibility. This helper takes care of such cases.
    fn item_is_effectively_public(&self, item: &Item) -> bool {
        if let Some(container) = self.item_id_to_container.get(&item.id) {
            match &container.inner {
                ItemEnum::Impl(i) => {
                    if let Some(implemented_trait) = match &i.trait_ {
                        Some(Type::ResolvedPath { id, .. }) => self.rustdoc_json.index.get(id),
                        _ => None,
                    } {
                        implemented_trait.visibility == Visibility::Public
                    } else {
                        false
                    }
                }

                // The item is contained in an enum, so it is an enum variant.
                // If the enum itself is public, then so are its variants. Since
                // the enum would not be in the rustdoc JSON if it was not
                // public, we know this variant is public.
                ItemEnum::Enum(_) => true,

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

    fn item_name_with_parents(&self, item: &Item, s: &mut String) {
        let effective_item_id = get_effective_id(item);
        if let Some(container) = item_id_to_container.get(effective_item_id) {
            item_name_with_parents(item_id_to_container, container, s);
            s.push_str(&format!("::{}", get_effective_name(item)));
        } else {
            s.push_str(&get_effective_name(item).to_string());
        }
    }

    fn container_for_item(&self, item: &Item) -> Option<&Item> {
        let effective_item_id = get_effective_id(item);
        self.item_id_to_container.get(effective_item_id)
    }
}

fn get_effective_id(item: &Item) -> &Id {
    match &item.inner {
        ItemEnum::Impl(i) => match &i.for_ {
            Type::ResolvedPath { id, .. } => id,
            _ => &item.id,
        },
        _ => &item.id,
    }
}

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

// impl has not a name, instead they are "for" something, but we want to print "for"
fn get_effective_name(item: &Item) -> &str {
    match &item.inner {
        ItemEnum::Import(i) => &i.name,
        ItemEnum::Impl(i) => match &i.for_ {
            Type::ResolvedPath { name, .. } => name.as_ref(),
            _ => item.name.as_ref().unwrap(),
        },
        _ => item.name.as_ref().unwrap(),
    }
}
