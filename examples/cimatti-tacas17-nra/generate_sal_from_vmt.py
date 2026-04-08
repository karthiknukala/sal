#!/usr/bin/env python3

"""Translate the TACAS'17 NRA VMT benchmarks into SAL.

This generator targets the 114 zero-arity VMT models in `benchmarks/vmt/nra`
from the Zenodo archive used by the ic3-nra evaluation. The generated SAL keeps
the VMT transition relation relational by placing the full init/trans formulas
in guarded commands and assigning every state variable nondeterministically.
"""

from __future__ import annotations

import argparse
import pathlib
import re
from dataclasses import dataclass
from typing import Iterable


RESERVED_WORDS = {
    "and",
    "array",
    "begin",
    "boolean",
    "claim",
    "context",
    "definition",
    "div",
    "else",
    "elsif",
    "end",
    "endif",
    "exists",
    "false",
    "forall",
    "global",
    "if",
    "implements",
    "importing",
    "in",
    "initialization",
    "input",
    "integer",
    "lambda",
    "lemma",
    "let",
    "local",
    "module",
    "mod",
    "natural",
    "not",
    "obligation",
    "observe",
    "or",
    "output",
    "real",
    "rename",
    "sync",
    "theorem",
    "then",
    "transition",
    "true",
    "type",
    "with",
    "xor",
}

SORT_MAP = {
    "Bool": "BOOLEAN",
    "Int": "INTEGER",
    "Real": "REAL",
}


@dataclass
class VariableDecl:
    raw_name: str
    sort: str


@dataclass
class StateVar:
    current_symbol: str
    next_symbol: str
    sort: str
    safe_name: str
    alias_name: str | None = None


@dataclass
class HelperDefinition:
    raw_name: str
    safe_name: str
    params: list[tuple[str, str]]
    return_sort: str
    body: object


class ParseError(RuntimeError):
    pass


class NameAllocator:
    def __init__(self) -> None:
        self.used: set[str] = set()

    def reserve(self, preferred: str) -> str:
        candidate = preferred
        counter = 2
        while candidate in self.used:
            candidate = f"{preferred}_{counter}"
            counter += 1
        self.used.add(candidate)
        return candidate


def strip_bars(token: str) -> str:
    if len(token) >= 2 and token[0] == "|" and token[-1] == "|":
        return token[1:-1]
    return token


def sanitize_identifier(raw: str) -> str:
    name = strip_bars(raw)
    name = re.sub(r"__AT[01]$", "", name)
    if name.endswith(".next"):
        name = name[:-5]
    name = name.replace("->", "_")
    name = re.sub(r"[^A-Za-z0-9_]+", "_", name)
    name = re.sub(r"_+", "_", name).strip("_").lower()
    if not name:
        name = "v"
    if name[0].isdigit():
        name = f"v_{name}"
    if name in RESERVED_WORDS:
        name = f"{name}_v"
    return name


def derive_state_base(current_symbol: str, next_symbol: str) -> str:
    current = strip_bars(current_symbol)
    next_name = strip_bars(next_symbol)
    if current.endswith("__AT0") and next_name.endswith("__AT1"):
        return sanitize_identifier(current[:-5])
    if next_name == f"{current}.next":
        return sanitize_identifier(current)
    return sanitize_identifier(current)


def tokenize(text: str) -> list[str]:
    tokens: list[str] = []
    index = 0
    length = len(text)
    while index < length:
        char = text[index]
        if char.isspace():
            index += 1
            continue
        if char == ";":
            while index < length and text[index] != "\n":
                index += 1
            continue
        if char in "()":
            tokens.append(char)
            index += 1
            continue
        if char == "|":
            end = index + 1
            while end < length and text[end] != "|":
                end += 1
            if end >= length:
                raise ParseError("unterminated |quoted identifier|")
            tokens.append(text[index : end + 1])
            index = end + 1
            continue
        end = index
        while end < length and (not text[end].isspace()) and text[end] not in "();":
            end += 1
        tokens.append(text[index:end])
        index = end
    return tokens


def parse_many(tokens: Iterable[str]) -> list[object]:
    stack: list[list[object]] = []
    current: list[object] = []
    for token in tokens:
        if token == "(":
            stack.append(current)
            current = []
        elif token == ")":
            if not stack:
                raise ParseError("unbalanced ')' in s-expression")
            completed = current
            current = stack.pop()
            current.append(completed)
        else:
            current.append(token)
    if stack:
        raise ParseError("unbalanced '(' in s-expression")
    return current


def unwrap_annotation(node: object) -> tuple[object, dict[str, object]]:
    if not isinstance(node, list) or not node or node[0] != "!":
        return node, {}
    if len(node) < 2:
        raise ParseError("malformed annotation")
    attrs: dict[str, object] = {}
    index = 2
    while index + 1 < len(node):
        key = node[index]
        value = node[index + 1]
        if not isinstance(key, str):
            raise ParseError("annotation key must be a symbol")
        attrs[key] = value
        index += 2
    return node[1], attrs


def list_to_pairs(items: list[object]) -> list[tuple[str, str]]:
    pairs: list[tuple[str, str]] = []
    for item in items:
        if not isinstance(item, list) or len(item) != 2:
            raise ParseError(f"expected parameter pair, got {item!r}")
        name, sort = item
        if not isinstance(name, str) or not isinstance(sort, str):
            raise ParseError(f"expected symbol parameter, got {item!r}")
        pairs.append((name, sort))
    return pairs


def is_numeric_atom(token: str) -> bool:
    return re.fullmatch(r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)", token) is not None


def convert_sort(sort: str) -> str:
    if sort not in SORT_MAP:
        raise ParseError(f"unsupported sort {sort}")
    return SORT_MAP[sort]


def fold_infix(operator: str, args: list[str]) -> str:
    if not args:
        raise ParseError(f"cannot emit empty {operator!r} expression")
    if len(args) == 1:
        return args[0]
    return f"({f' {operator} '.join(args)})"


def chain_rel(operator: str, args: list[str]) -> str:
    if len(args) < 2:
        raise ParseError(f"relation {operator!r} needs at least two arguments")
    if len(args) == 2:
        return f"({args[0]} {operator} {args[1]})"
    return fold_infix("AND", [f"({lhs} {operator} {rhs})" for lhs, rhs in zip(args, args[1:])])


class Translator:
    def __init__(self, path: pathlib.Path) -> None:
        self.path = path
        self.forms = parse_many(tokenize(path.read_text()))
        self.declarations: dict[str, VariableDecl] = {}
        self.state_vars: list[StateVar] = []
        self.state_by_current: dict[str, StateVar] = {}
        self.state_by_next: dict[str, StateVar] = {}
        self.symbol_refs: dict[str, str] = {}
        self.helper_refs: dict[str, str] = {}
        self.param_helpers: list[HelperDefinition] = []
        self.module_helpers: list[HelperDefinition] = []
        self.definition_helpers: list[HelperDefinition] = []
        self.transition_helpers: list[HelperDefinition] = []
        self.helper_dependencies: dict[str, set[str]] = {}
        self.init_expr: object | None = None
        self.trans_expr: object | None = None
        self.prop_expr: object | None = None
        self.context_name = sanitize_identifier(path.stem)

    def collect(self) -> None:
        allocator = NameAllocator()

        state_aliases: dict[str, tuple[str, str, str]] = {}
        for form in self.forms:
            if not isinstance(form, list) or not form:
                continue
            head = form[0]
            if head == "declare-const":
                if len(form) != 3:
                    raise ParseError(f"malformed declare-const in {self.path.name}")
                name = form[1]
                sort = form[2]
                if not isinstance(name, str) or not isinstance(sort, str):
                    raise ParseError(f"bad declaration in {self.path.name}")
                self.declarations[name] = VariableDecl(name, sort)
            elif head == "declare-fun":
                if len(form) != 4:
                    raise ParseError(f"malformed declare-fun in {self.path.name}")
                name = form[1]
                args = form[2]
                sort = form[3]
                if not isinstance(name, str) or not isinstance(sort, str) or not isinstance(args, list):
                    raise ParseError(f"bad declaration in {self.path.name}")
                if args:
                    raise ParseError(f"unsupported non-zero-arity declaration {name} in {self.path.name}")
                self.declarations[name] = VariableDecl(name, sort)
            elif head == "define-fun":
                if len(form) != 5:
                    raise ParseError(f"malformed define-fun in {self.path.name}")
                name = form[1]
                params = form[2]
                sort = form[3]
                body = form[4]
                if not isinstance(name, str) or not isinstance(params, list) or not isinstance(sort, str):
                    raise ParseError(f"bad define-fun in {self.path.name}")
                if params:
                    continue
                expr, attrs = unwrap_annotation(body)
                next_symbol = attrs.get(":next")
                if next_symbol is not None:
                    if not isinstance(expr, str) or not isinstance(next_symbol, str):
                        raise ParseError(f"unsupported :next wrapper in {self.path.name}")
                    state_aliases[name] = (expr, next_symbol, sort)

        for alias_name, (current_symbol, next_symbol, sort) in state_aliases.items():
            safe_name = allocator.reserve(derive_state_base(current_symbol, next_symbol))
            state = StateVar(
                current_symbol=current_symbol,
                next_symbol=next_symbol,
                sort=sort,
                safe_name=safe_name,
                alias_name=alias_name,
            )
            self.state_vars.append(state)
            self.state_by_current[current_symbol] = state
            self.state_by_next[next_symbol] = state
            self.symbol_refs[current_symbol] = safe_name
            self.symbol_refs[next_symbol] = f"{safe_name}'"
            self.helper_refs[alias_name] = safe_name

        for state in self.state_vars:
            if state.current_symbol not in self.declarations:
                self.declarations[state.current_symbol] = VariableDecl(state.current_symbol, state.sort)
            if state.next_symbol not in self.declarations:
                self.declarations[state.next_symbol] = VariableDecl(state.next_symbol, state.sort)

        for raw_name, decl in self.declarations.items():
            if raw_name in self.state_by_current or raw_name in self.state_by_next:
                continue
            safe_name = allocator.reserve(sanitize_identifier(raw_name))
            self.symbol_refs[raw_name] = safe_name

        for form in self.forms:
            if not isinstance(form, list) or not form or form[0] != "define-fun":
                continue
            name = form[1]
            params = list_to_pairs(form[2])
            sort = form[3]
            body = form[4]
            if not isinstance(name, str) or not isinstance(sort, str):
                raise ParseError(f"bad define-fun in {self.path.name}")

            if not params and name in state_aliases:
                continue

            expr, attrs = unwrap_annotation(body)
            safe_name = allocator.reserve(sanitize_identifier(name))
            helper = HelperDefinition(
                raw_name=name,
                safe_name=safe_name,
                params=params,
                return_sort=sort,
                body=expr,
            )
            self.helper_refs[name] = safe_name
            if ":init" in attrs:
                self.module_helpers.append(helper)
                self.init_expr = name
                continue
            if ":trans" in attrs:
                self.module_helpers.append(helper)
                self.trans_expr = name
                continue
            if ":invar-property" in attrs:
                self.module_helpers.append(helper)
                self.prop_expr = name
                continue
            if params:
                self.param_helpers.append(helper)
            else:
                self.module_helpers.append(helper)

        if self.init_expr is None or self.trans_expr is None or self.prop_expr is None:
            raise ParseError(f"missing init/trans/property in {self.path.name}")

        self.partition_module_helpers()

    def collect_symbol_atoms(self, expr: object) -> set[str]:
        atoms: set[str] = set()
        if isinstance(expr, str):
            bare = strip_bars(expr)
            if bare not in {"true", "false"} and not is_numeric_atom(bare):
                atoms.add(expr)
            return atoms
        if not isinstance(expr, list):
            return atoms
        for item in expr:
            atoms.update(self.collect_symbol_atoms(item))
        return atoms

    def partition_module_helpers(self) -> None:
        helper_by_raw = {helper.raw_name: helper for helper in self.module_helpers}
        next_sensitive: set[str] = set()
        dependencies: dict[str, set[str]] = {}

        for helper in self.module_helpers:
            atoms = self.collect_symbol_atoms(helper.body)
            dependencies[helper.raw_name] = {atom for atom in atoms if atom in helper_by_raw}
            if any(atom in self.state_by_next for atom in atoms):
                next_sensitive.add(helper.raw_name)

        changed = True
        while changed:
            changed = False
            for raw_name, deps in dependencies.items():
                if raw_name in next_sensitive:
                    continue
                if deps & next_sensitive:
                    next_sensitive.add(raw_name)
                    changed = True

        self.definition_helpers = [helper for helper in self.module_helpers if helper.raw_name not in next_sensitive]
        self.transition_helpers = [helper for helper in self.module_helpers if helper.raw_name in next_sensitive]
        self.helper_dependencies = dependencies

        prop_name = self.prop_expr if isinstance(self.prop_expr, str) else None
        if prop_name in next_sensitive:
            raise ParseError(f"property helper {prop_name} unexpectedly depends on next-state variables in {self.path.name}")

    def emit_atom(self, token: str, local_env: dict[str, str]) -> str:
        bare = strip_bars(token)
        if bare == "true":
            return "TRUE"
        if bare == "false":
            return "FALSE"
        if is_numeric_atom(bare):
            return bare
        if token in local_env:
            return local_env[token]
        if token in self.symbol_refs:
            return self.symbol_refs[token]
        if token in self.helper_refs:
            return self.helper_refs[token]
        raise ParseError(f"unbound symbol {token!r} in {self.path.name}")

    def emit_expr(self, expr: object, local_env: dict[str, str] | None = None) -> str:
        env = local_env or {}
        if isinstance(expr, str):
            return self.emit_atom(expr, env)
        if not isinstance(expr, list) or not expr:
            raise ParseError(f"malformed expression {expr!r} in {self.path.name}")

        head = expr[0]
        if head == "!":
            return self.emit_expr(unwrap_annotation(expr)[0], env)
        if not isinstance(head, str):
            raise ParseError(f"unexpected expression head {head!r} in {self.path.name}")

        args = [self.emit_expr(item, env) for item in expr[1:]]
        if head == "and":
            return fold_infix("AND", args)
        if head == "or":
            return fold_infix("OR", args)
        if head == "not":
            if len(args) != 1:
                raise ParseError(f"'not' expects one argument in {self.path.name}")
            return f"(NOT {args[0]})"
        if head == "=>":
            if len(args) != 2:
                raise ParseError(f"'=>' expects two arguments in {self.path.name}")
            return f"({args[0]} => {args[1]})"
        if head == "=":
            return chain_rel("=", args)
        if head == "<":
            return chain_rel("<", args)
        if head == "<=":
            return chain_rel("<=", args)
        if head == ">":
            return chain_rel(">", args)
        if head == ">=":
            return chain_rel(">=", args)
        if head == "+":
            return fold_infix("+", args)
        if head == "*":
            return fold_infix("*", args)
        if head == "-":
            if len(args) == 1:
                return f"(-({args[0]}))"
            return fold_infix("-", args)
        if head == "/":
            return fold_infix("/", args)
        if head == "ite":
            if len(args) != 3:
                raise ParseError(f"'ite' expects three arguments in {self.path.name}")
            return f"(IF {args[0]} THEN {args[1]} ELSE {args[2]} ENDIF)"
        if head == "to_real":
            if len(args) != 1:
                raise ParseError(f"'to_real' expects one argument in {self.path.name}")
            return args[0]

        callee = self.emit_atom(head, env)
        return f"{callee}({', '.join(args)})"

    def render_function_helper(self, helper: HelperDefinition) -> str:
        local_allocator = NameAllocator()
        local_env: dict[str, str] = {}
        rendered_params: list[str] = []
        for raw_name, sort in helper.params:
            safe_name = local_allocator.reserve(sanitize_identifier(raw_name))
            local_env[raw_name] = safe_name
            rendered_params.append(f"{safe_name}: {convert_sort(sort)}")
        body = self.emit_expr(helper.body, local_env)
        return f"  {helper.safe_name}({', '.join(rendered_params)}): {convert_sort(helper.return_sort)} = {body};"

    def render_module_helper(self, helper: HelperDefinition) -> str:
        return f"    {helper.safe_name} = {self.emit_expr(helper.body)};"

    def render_decls(self, label: str, names: list[tuple[str, str]]) -> list[str]:
        if not names:
            return []
        lines = [f"    {safe}: {sort}" for safe, sort in names]
        for index in range(len(lines) - 1):
            lines[index] += ","
        return [f"  {label}"] + lines

    def render_guarded_assignments(self, init_mode: bool) -> list[str]:
        assignments: list[str] = []
        for state in self.state_vars:
            binder = f"v_{state.safe_name}"
            lhs = state.safe_name if init_mode else f"{state.safe_name}'"
            assignments.append(f"        {lhs} IN {{ {binder}: {convert_sort(state.sort)} | TRUE }};")
        if assignments:
            assignments[-1] = assignments[-1].rstrip(";")
        return assignments

    def collect_needed_transition_helpers(self, expr: object) -> set[str]:
        needed = {atom for atom in self.collect_symbol_atoms(expr) if atom in {helper.raw_name for helper in self.transition_helpers}}
        changed = True
        while changed:
            changed = False
            for raw_name in list(needed):
                deps = self.helper_dependencies.get(raw_name, set())
                missing = {dep for dep in deps if dep in {helper.raw_name for helper in self.transition_helpers} and dep not in needed}
                if missing:
                    needed.update(missing)
                    changed = True
        return needed

    def render_transition_guard(self) -> str:
        guard_expr = self.emit_expr(self.trans_expr)
        needed = self.collect_needed_transition_helpers(self.trans_expr)
        for helper in reversed(self.transition_helpers):
            if helper.raw_name not in needed:
                continue
            body = self.emit_expr(helper.body)
            guard_expr = f"LET {helper.safe_name}: {convert_sort(helper.return_sort)} = {body} IN\n        {guard_expr}"
        return guard_expr

    def render(self) -> str:
        function_helpers = [self.render_function_helper(helper) for helper in self.param_helpers]
        input_decls = sorted(
            [(safe, convert_sort(self.declarations[raw].sort)) for raw, safe in self.symbol_refs.items() if raw not in self.state_by_current and raw not in self.state_by_next],
            key=lambda item: item[0],
        )
        output_decls = [(state.safe_name, convert_sort(state.sort)) for state in self.state_vars]
        local_decls = [(helper.safe_name, convert_sort(helper.return_sort)) for helper in self.definition_helpers]
        definition_helpers = [self.render_module_helper(helper) for helper in self.definition_helpers]

        lines = [
            f"% Auto-generated from {self.path.name}",
            f"% Source suite: benchmarks/vmt/nra/{self.path.name}",
            f"{self.context_name}: CONTEXT =",
            "BEGIN",
        ]
        if function_helpers:
            lines.extend(function_helpers)
            lines.append("")

        lines.extend(
            [
                "  system: MODULE =",
                "  BEGIN",
            ]
        )
        lines.extend(self.render_decls("INPUT", input_decls))
        if input_decls:
            lines.append("")
        lines.extend(self.render_decls("OUTPUT", output_decls))
        if output_decls and local_decls:
            lines.append("")
        lines.extend(self.render_decls("LOCAL", local_decls))
        if (output_decls or local_decls) and definition_helpers:
            lines.append("")
        if definition_helpers:
            lines.append("  DEFINITION")
            lines.extend(definition_helpers)
            lines.append("")

        lines.extend(
            [
                "  INITIALIZATION",
                "  [",
                f"      {self.emit_expr(self.init_expr)} -->",
            ]
        )
        lines.extend(self.render_guarded_assignments(init_mode=True))
        lines.extend(
            [
                "  ]",
                "",
                "  TRANSITION",
                "  [",
                f"      {self.render_transition_guard()} -->",
            ]
        )
        lines.extend(self.render_guarded_assignments(init_mode=False))
        lines.extend(
            [
                "  ]",
                "  END;",
                "",
                f"  property: THEOREM system |- G({self.emit_expr(self.prop_expr)});",
                "END",
                "",
            ]
        )
        return "\n".join(lines)


def translate_file(src_path: pathlib.Path, dst_path: pathlib.Path) -> None:
    translator = Translator(src_path)
    translator.collect()
    dst_path.write_text(translator.render())


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    script_dir = pathlib.Path(__file__).resolve().parent
    parser.add_argument(
        "--src-dir",
        type=pathlib.Path,
        default=script_dir / "vmt",
        help="directory containing the 114 source VMT files",
    )
    parser.add_argument(
        "--dst-dir",
        type=pathlib.Path,
        default=script_dir / "sal",
        help="directory for generated SAL files",
    )
    parser.add_argument(
        "--clean",
        action="store_true",
        help="remove existing generated SAL files before translating",
    )
    parser.add_argument(
        "--manifest",
        type=pathlib.Path,
        default=None,
        help="optional manifest path (defaults to <dst-dir>/MANIFEST.tsv)",
    )
    args = parser.parse_args()

    src_dir = args.src_dir.resolve()
    dst_dir = args.dst_dir.resolve()
    manifest_path = args.manifest.resolve() if args.manifest else (dst_dir / "MANIFEST.tsv")
    if not src_dir.is_dir():
        raise SystemExit(f"source directory does not exist: {src_dir}")

    dst_dir.mkdir(parents=True, exist_ok=True)
    if args.clean:
        for path in dst_dir.glob("*.sal"):
            path.unlink()
        if manifest_path.exists():
            manifest_path.unlink()

    sources = sorted(src_dir.glob("*.vmt"))
    if len(sources) != 114:
        raise SystemExit(f"expected 114 VMT files in {src_dir}, found {len(sources)}")

    manifest_lines = ["source_vmt\tsal_file\tcontext_name"]
    for src_path in sources:
        translator = Translator(src_path)
        translator.collect()
        dst_path = dst_dir / f"{translator.context_name}.sal"
        dst_path.write_text(translator.render())
        manifest_lines.append(f"{src_path.name}\t{dst_path.name}\t{translator.context_name}")

    manifest_path.write_text("\n".join(manifest_lines) + "\n")


if __name__ == "__main__":
    main()
