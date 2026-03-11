# ocaml-data-transform

OCaml-based data transformation pipeline using protobuf for typed schema definitions. Transforms structured JSON input data between source and target table schemas using configurable field mappings (textbox, checkbox, custom). Protobuf defines both the data type system (simple + compound types) and per-table instance schemas, which compile to OCaml types for compile-time field access safety.

## Prerequisites

- OCaml >= 5.4
- opam
- dune >= 3.16
- ocaml-protoc v4.0

### opam packages

| Package | Purpose |
|---------|---------|
| `yojson` >= 2.0 | JSON parsing/generation |
| `re` >= 1.11 | Regular expressions |
| `pbrt` | Protobuf binary runtime |
| `pbrt_yojson` | Protobuf JSON runtime |
| `alcotest` >= 1.7 | Test framework (test-only) |

## Setup

```bash
opam install . --deps-only --with-test
dune build
```

## Running Tests

```bash
dune runtest
```

26 tests across 7 suites: schema, protobuf, json_path, transform_utils, mappings, example_mappings, integration.

## Project Structure

```
proto/
├── data_types.proto                  # Type system: 6 simple + 16 compound types + CustomCompoundType
├── data_types_constants.json         # Runtime metadata for 47 data types (regex, options, etc.)
├── table_schema.proto                # Generic table schema with SingleFieldType oneof
└── tables/
    ├── source_table.proto            # Typed source schema (7 fields, explicit compound sub-fields)
    ├── source_table_constants.json   # Source field metadata (typeIds, option keys/labels)
    ├── target_table.proto            # Typed target schema (8 fields)
    └── target_table_constants.json   # Target field metadata (typeIds, option keys/labels)
lib/
├── dune                              # Build rules incl. ocaml-protoc codegen + sed fixes
├── schema.ml / .mli                  # Schema loading (data type constants + table schemas)
├── json_path.ml / .mli              # JSON path navigation
├── transform_utils.ml / .mli        # Collection utilities
├── mappings.ml / .mli               # Mapping engine
└── example_mappings.ml / .mli       # Concrete field mappings
test/
├── fixtures/
│   ├── values.json                   # Sample source input
│   └── transformed_values.json       # Expected transform output
├── test_schema.ml                    # Schema + table schema loading tests
├── test_protobuf.ml                  # Binary/JSON roundtrip tests
├── test_json_path.ml                 # JSON path tests
├── test_transform_utils.ml           # Utility function tests
├── test_mappings.ml                  # Mapping engine tests
├── test_example_mappings.ml          # split_name + commitment mapping tests
├── test_integration.ml               # End-to-end pipeline test
└── test_main.ml                      # Test runner entry point
```

## Main Components

### Proto Layer

**`data_types.proto`** — Defines the type system. Simple types (`StringType`, `NumberType`, `BooleanType`, `EnumType`, `MultipleCheckboxType`, `RadioGroupType`) and 15 compound types following the `*SubFields` + wrapper pattern (e.g., `AddressSubFields` / `AddressType`). `CustomCompoundType` uses `map<string, NonCustomFieldValue>` for flexible sub-fields.

**`table_schema.proto`** — Generic `TableSchema` with `map<string, SingleFieldType>` where `SingleFieldType` is a oneof of all 22 data types. Used for dynamic/generic table operations.

**`tables/source_table.proto`** / **`target_table.proto`** — Instance-specific typed schemas. Each table's `FieldsMap` message has named, typed fields. `CustomCompoundType` instances (e.g., `lp_signatory`) get explicit sub-field messages (`LpSignatoryFields`). Known compound types are used directly.

### Schema (`schema.ml`)

Loads runtime metadata from JSON constants files.

- `load_data_type_constants : unit -> data_type_constants` — 47 type definitions with regex, format patterns, options
- `load_source_table_schema : unit -> Source_table.source_table_schema` — Typed source schema
- `load_target_table_schema : unit -> Target_table.target_table_schema` — Typed target schema

### Json_path (`json_path.ml`)

JSON navigation utilities for nested path access.

- `get_path` / `set_path` — Navigate/set by string key path
- `get_string` / `get_string_or_empty` / `get_string_list` — Typed extraction

### Transform_utils (`transform_utils.ml`)

Collection utilities for data transformation.

- `group_by` / `merge_by` — Group/merge lists by key function
- `map_values` — Map over key-value pair values
- `deep_merge` — Recursively merge JSON objects

### Mappings (`mappings.ml`)

Core mapping engine. Three mapping types:

- `textbox_mapping` — Extracts first non-empty string value from input paths
- `checkbox_mapping` — Maps selected keys through an option_map to produce output keys
- `custom_mapping` — Arbitrary transform function over collected inputs

Key functions: `apply_mapping` (single), `transform_all` (batch with deep_merge).

### Example_mappings (`example_mappings.ml`)

6 concrete field mappings: commitment, investor_name, regulated_status, international_supplements, signer_name (with `split_name` helper), w9_tin_type.

`all_mappings : unit -> mapping list` returns the full mapping set.

## Data Flow

```
values.json (source)
    │
    ▼
Example_mappings.all_mappings()     ← defines which fields map where
    │
    ▼
Mappings.transform_all              ← applies each mapping, deep-merges results
    │
    ▼
transformed_values.json (target)    ← 8 output fields
```

## Known Issues

### ocaml-protoc `map<>` yojson codegen bug

ocaml-protoc v4.0 generates broken OCaml code for `map<>` fields with `--yojson`: it emits `assoc :=` instead of `assoc := (` before `let` bindings. The dune build rules apply a sed fix:

```bash
sed -i "" "s/assoc :=$/assoc := (/" data_types.ml
```

This affects `data_types.ml` (CustomCompoundType) and `table_schema.ml` (TableSchema.fields_map).

### ocaml-protoc JSON field naming (camelCase vs snake_case)

ocaml-protoc always emits camelCase keys in JSON encoding, ignoring the `json_name` proto option. The naming flow:

| Layer | Convention | Example |
|-------|-----------|---------|
| Proto field name | snake_case | `lp_signatory` |
| OCaml record field | snake_case | `lp_signatory` |
| JSON key (generated) | camelCase | `lpSignatory` |

Constants JSON files use **snake_case** for readability (matching proto and OCaml). `schema.ml` includes a `snake_to_camel` JSON key preprocessor that converts keys before passing to the generated decoders.

String values in `field_keys_in_order` preserve the original logical field keys as-is (which may be mixed-case, e.g., `sf_Agreement_null_Commitment_c`).
