# Common deterministic development contract

These instructions apply to `D:\DEV\Common`, the shared F#/.NET dependency used
by Aiarwa and JCS.

## Scope and dependency impact

- Common is the default write boundary. Consumer repositories are read-only
  unless the user explicitly authorizes a separate cross-repository task.
- Preserve pre-existing work and inspect `git status --short` before editing.
- Before changing a public type, function, project reference, deployment API,
  or serialized contract, search all consumers under `D:\DEV` and state the
  compatibility impact.
- Do not add project-specific business behavior to a general utility. Put it in
  the consuming application unless it is genuinely reusable.

## F# conventions

- Order `open` declarations by dependency: System, third party, Util, shared
  libraries, project modules, then the current namespace, with blank lines
  between groups.
- Prefer interpolation to `sprintf`, type inference to redundant annotations,
  curried calls without unnecessary parentheses, and `|> ignore` for ignored
  collection mutation results. Use `new` for class construction.

## High-risk areas

- `UtilKestrel` deployment and service-management changes require matching
  Aiarwa compatibility verification.
- Database, service, SSH, deployment, publication, and package publishing
  actions require an explicit current user request.
- Never expose credentials or connection strings found in source/config.

## Verification

- Run `scripts\codex-verify.ps1`; use `-Full` for public API, project-reference,
  or deployment changes.
- The normal backend gate is a Release build of `Common-Codex.slnf`, which
  covers the active net10 libraries used by Aiarwa and TypeSys without
  requiring legacy Zmq/web or mobile workloads.
  Changes under `UtilMaui`, `MauiFs`, or `MauiFsLogics` additionally require
  `Common-All.sln` and the Android/MAUI workloads. Run focused tests for touched
  libraries.
- A failed required check blocks a successful-completion claim.

## Code review rules

- Flag consumer-breaking API changes without compatibility evidence, single-app
  special cases in Common, unsafe deployment behavior, secret exposure, and
  missing focused tests for shared logic.
