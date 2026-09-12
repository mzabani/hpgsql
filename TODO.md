- Test both `singleField fieldDecoder` and `singleFieldRowDecoder` for every type in our tests.
- Some types might still not derive specialized row decoders
  - And rewrite rules too are missing for many
- Text internals usage.. is it safe? Double-check.
- Check that we're not holding on to internal buffers when Record fields being materialized into aren't strict
- Write property-based tests for PinnedByteArray functions
- Do expose `notInlinedSingleFieldRowDecoder` because rewrite rules are not so reliable. If users fmap over field decoders or compose over them, our rewrite rules might not apply. They might also not apply when compiling with `-O0`.
  - Just think of better method names.
  - Also amend our docs of the multiple ways to derive row decoders.

