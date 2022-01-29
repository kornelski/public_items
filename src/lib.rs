use std::ops::Index;

use rustdoc_types::Crate;

mod error;
mod public_item_builder;

/// The crate error type.
pub use error::Error;

/// The crate result type.
pub use error::Result;
use rustdoc_types::Id;
use rustdoc_types::Item;

/// Takes rustdoc JSON and returns a [`Vec`] of [`String`]s where each
/// [`String`] is one public item of the crate, i.e. part of the crate's public
/// API. The [`Vec`] is sorted in a way suitable for display to humans, but the
/// exact order is unspecified.
///
/// There exists a convenient `cargo` wrapper for this function found at
/// <https://github.com/Enselic/cargo-public-items> that builds the rustdoc JSON
/// for you and then invokes this function. If you don't want to use that
/// wrapper, use
/// ```bash
/// RUSTDOCFLAGS='-Z unstable-options --output-format json' cargo +nightly doc --lib --no-deps
/// ```
/// to generate the rustdoc JSON that this function takes as input. For
/// reference, the rustdoc JSON format is documented at
/// <https://rust-lang.github.io/rfcs/2963-rustdoc-json.html>.
///
/// # Errors
///
/// E.g. if the JSON is invalid.
pub fn sorted_public_items_from_rustdoc_json_str(rustdoc_json_str: &str) -> crate::Result<Vec<String>> {
    let crate_: Crate = serde_json::from_str(rustdoc_json_str)?;
    let root_item = crate_.index.get(&crate_.root).ok_or(Error::NoRootItemFound)?;

    let items = recursively_collect_child_items_from(root_item);

    let builder = public_item_builder::PublicItemBuilder::new(&crate_);

    let mut result: Vec<String> = crate_
        .index
        .values()
        .filter(|item| item_is_relevant(item))
        .map(|item| builder.build_from_item(item))
        .collect();

    result.sort();

    Ok(result)
}

struct IndexIterator<'a> {
    crate_: &'a Crate,
    // visited_items: std::collections::HashSet<Id>, TODO: Needed? Can recurse?
    ids_left_to_visit: Vec<Id>,
}

impl<'a> IndexIterator<'a> {
    fn new(crate_: &'a Crate) -> Self {
        IndexIterator {
            crate_,
            ids_left_to_visit: vec![crate_.root],
        }
    }
}

impl<'a> Iterator for IndexIterator<'a> {
    type Item = &'a Item;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

struct Im {
    path: Vec<Item>,
}

impl Im {
    fn print_item(item: &Item) {
        
    }
}

fn recursively_collect_child_items_from(root_item: &Item) -> Vec<&Item> {
    let result = vec![];
 
    let mut items_left_to_process = vec![root_item];
 
    while let Some(item) = items_left_to_process.pop() {
        if let Some(items) = items_in_container(&item) {
            items_left_to_process.extend(items);
 
        }
    }
    result.push(root_item);
 
 
 
    result
}
