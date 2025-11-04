# Research: Pattern Data Structure

**Feature**: 001-pattern-data-structure  
**Date**: 2025-01-27

## Decisions

### GHC Version

**Decision**: Use GHC 9.8.2 (or latest stable 9.8.x) as the target version.

**Rationale**: 
- Current installed version is 8.10.7, which is quite outdated (released 2021)
- GHC 9.10.1 is the latest but may have compatibility issues with some libraries
- GHC 9.8.2 provides a good balance of modern features and library compatibility
- 9.8.x series is stable and well-supported
- Reference implementations benefit from modern GHC features (better type system, performance improvements)

**Alternatives considered**:
- GHC 8.10.7 (current): Too old, missing modern features
- GHC 9.10.1 (latest): May have library compatibility issues, too new for some dependencies
- GHC 9.6.x: Good alternative, but 9.8.x has better long-term support

**Action**: Upgrade GHC to 9.8.2 using `ghcup install ghc 9.8.2`

---

### Build Tool

**Decision**: Use Cabal as the primary build tool.

**Rationale**:
- Cabal is the official build tool that comes with GHC
- Standard for library projects in the Haskell ecosystem
- Better for reference implementations (more portable, less opinionated)
- Simpler configuration for library projects
- Already available (Cabal 3.6.2.0 installed, can upgrade to 3.12.1.0)

**Alternatives considered**:
- Stack: Good for applications with complex dependency management, but adds abstraction layer
- Nix: Powerful but complex, not necessary for a reference library implementation

**Action**: Use `cabal init` to initialize the project. Consider upgrading Cabal to 3.12.1.0 for latest features.

---

### Testing Framework

**Decision**: Use QuickCheck for property-based testing, with HSpec for unit tests.

**Rationale**:
- QuickCheck is the de facto standard for property-based testing in Haskell
- Excellent for testing category-theoretic properties (functor laws, naturality conditions)
- Well-documented and widely used in the Haskell community
- HSpec provides clear, readable unit test syntax
- Both integrate well with Cabal

**Alternatives considered**:
- Hedgehog: Modern alternative with better shrinking, but less widely adopted
- HUnit: Too basic, lacks property-based testing capabilities
- Doctest: Good for documentation examples, but not sufficient for comprehensive testing

**Action**: Add `QuickCheck` and `hspec` as test dependencies.

---

### Category Theory Libraries

**Decision**: Start with base libraries only; add specialized libraries as needed.

**Rationale**:
- Pattern type is a recursive data structure that can be implemented with standard Haskell features
- Category-theoretic concepts (functors, morphisms) are represented through typeclasses and functions
- No immediate need for external category theory libraries (like `category-extras`)
- Keep dependencies minimal for a reference implementation
- Can add libraries later if needed (e.g., `lens` for traversals, `recursion-schemes` for recursion patterns)

**Alternatives considered**:
- `lens`: Powerful but complex, may be overkill initially
- `recursion-schemes`: Useful for recursion patterns, but can be added later if needed
- `category-extras`: Specialized category theory library, not needed for basic functor/morphism patterns

**Action**: Start with minimal dependencies. Add libraries as the design evolves.

---

### Project Structure

**Decision**: Use standard Cabal library structure with modular organization.

**Rationale**:
- Follows Haskell best practices and conventions
- Easy to understand for developers familiar with Haskell
- Supports clean separation of concerns (Core, Views, Graph, Morphisms)
- Test structure mirrors source structure for clarity
- Standard structure is more portable and easier to translate to other languages

**Alternatives considered**:
- Monolithic single module: Not scalable, doesn't reflect conceptual structure
- Flat structure: Less organized, harder to navigate

**Action**: Use `src/Pattern/` for source modules, `tests/Spec/Pattern/` for tests.

---

### Performance Goals

**Decision**: Performance is not a primary concern for the initial reference implementation.

**Rationale**:
- This is a reference implementation focused on correctness and clarity
- Mathematical correctness (category-theoretic laws) is paramount
- Performance can be optimized later if needed
- Reference implementations prioritize clarity over performance

**Alternatives considered**:
- Set specific performance targets: Premature optimization, not needed for reference design
- Benchmark from the start: Adds complexity without clear benefit initially

**Action**: Focus on correctness and clarity. Add performance benchmarks later if needed.

---

## Tooling Status

### Current Versions
- **GHC**: 8.10.7 (installed) - **RECOMMENDED**: 9.8.2
- **Cabal**: 3.6.2.0 (installed) - **RECOMMENDED**: 3.12.1.0
- **Stack**: 2.7.5 (installed) - **RECOMMENDED**: 2.15.7 (optional, not needed if using Cabal)
- **HLS**: 1.7.0.0 (installed) - **RECOMMENDED**: 2.9.0.1

### Upgrade Recommendations
1. Upgrade GHC to 9.8.2 for modern features
2. Upgrade Cabal to 3.12.1.0 for latest build tool features
3. Upgrade HLS to 2.9.0.1 for better IDE support
4. Stack upgrade is optional (not needed if using Cabal)

---

## Dependencies (Initial)

### Build Dependencies
- `base` >= 4.17.0.0 (comes with GHC 9.8.2)
- `containers` (for Set operations in undirected graph views)
- `transformers` (if needed for advanced patterns)

### Test Dependencies
- `QuickCheck` (property-based testing)
- `hspec` (unit testing framework)
- `hspec-quickcheck` (QuickCheck integration with HSpec)

### Development Dependencies
- `hspec-discover` (auto-discovery of test files)

---

## Summary

All NEEDS CLARIFICATION items from Technical Context have been resolved:

1. ✅ **GHC Version**: 9.8.2 (target)
2. ✅ **Build Tool**: Cabal (primary)
3. ✅ **Testing**: QuickCheck + HSpec
4. ✅ **Dependencies**: Minimal start, add as needed
5. ✅ **Performance**: Not primary concern for reference implementation
6. ✅ **Project Structure**: Standard Cabal library structure

The project is ready to proceed with Phase 1 design artifacts.

