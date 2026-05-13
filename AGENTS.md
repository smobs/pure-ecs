# Agent Notes

This is a pure functional Entity Component System for PureScript. The
authoritative implementation guide lives in [CLAUDE.md](CLAUDE.md) — start
there for project conventions, design philosophy, and migration notes.

When working on this codebase:

- Read `CLAUDE.md` first for API patterns and the system layer's contract.
- See `src/ECS/CLAUDE.md` for module-local guidance scoped to `src/ECS/`.
- Run tests with `npm test` (the `spago test` path is broken under
  `purs-backend-es`).
- Example pipeline lives at `src/ECS/Examples/SimpleExample.purs`; the
  generated pipeline doc lives at `docs/example-pipeline.md`.

<!-- pure-ecs:gameTick:begin -->

### ECS pipeline: `gameTick`

Auto-generated docs: [docs/example-pipeline.md](docs/example-pipeline.md)

**Execution order:** physics → damage → cleanup

Managed by `ECS.Docs.Write`; do not hand-edit.
<!-- pure-ecs:gameTick:end -->
