use thiserror::Error;

#[derive(Error, Debug)]
#[non_exhaustive] // We reserve the right to add more enum variants
pub enum Error {
    #[error(transparent)]
    SerdeJsonError(#[from] serde_json::Error),
    #[error("No root item found")]
    NoRootItemFound,
}

pub type Result<T> = std::result::Result<T, Error>;
