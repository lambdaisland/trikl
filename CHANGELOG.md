# Unreleased

## Added

- `stdio-client` for a direct STDIN/STDOUT connection
- Added access to screen and bounding box dimensions during rendering

## Fixed

- Fix re-rendering when the terminal size changes, before there would often be
  artifacts.
- Fix endless loop when client socket closes

## Changed
