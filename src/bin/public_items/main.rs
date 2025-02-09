use std::ffi::OsStr;
use std::io::Write;
use std::path::Path;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    match std::env::args_os().nth(1) {
        Some(first_arg) => handle_first_arg(&first_arg)?,
        _ => print_usage()?,
    }

    Ok(())
}

fn handle_first_arg(first_arg: &OsStr) -> Result<()> {
    if first_arg == "--help" || first_arg == "-h" {
        print_usage()?;
    } else {
        print_public_api_items(Path::new(&first_arg))?;
    }

    Ok(())
}

fn print_public_api_items(path: &Path) -> Result<()> {
    let json = &std::fs::read_to_string(path)?;

    for public_item in public_items::sorted_public_items_from_rustdoc_json_str(json)? {
        writeln!(std::io::stdout(), "{}", public_item)?;
    }

    Ok(())
}

fn print_usage() -> std::io::Result<()> {
    writeln!(
        std::io::stdout(),
        r"
NOTE: See https://github.com/Enselic/cargo-public-items for a convenient cargo
wrapper around this program (or to be precise; library) that does everything
automatically.

If you insist of using this low-level utility, you run it like this:

   public_items RUSTDOC_JSON_FILE

where RUSTDOC_JSON_FILE is the path to the output of

  RUSTDOCFLAGS='-Z unstable-options --output-format json' cargo +nightly doc --lib --no-deps

which you can find in

  ./target/doc/${{CRATE}}.json
"
    )
}
