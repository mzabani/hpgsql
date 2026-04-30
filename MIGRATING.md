hpgsql-simple-compat tries as much as possible to provide an API that is the same as postgresql-simple's and "just works", but that is not possible to do completely.

Even if your application compiles with hpgsql-simple-compat, **you must test every query** because there are some run time errors possible.

> [!INFO]
> You should start by swapping all of "postgresql-simple", "postgresql-libpq", and "postgresql-query" in your cabal files by "hpgsql-simple-compat".
> Then try to compile your project. It's unlikely it'll build, but start with that.

You can also add "hpgsql" to your cabal files, and then you can start migrating your queries one at a time by using the imports from "Hpgsql.*", and taking an `HPgConnection` from the hpgsql-simple-compat connection with the `Database.PostgreSQL.Simple.hpgConn` function.

If you are compiling your project, some things you might run into:

### No instance for ‘Hpgsql.Encoding.ToPgField SomeType`

You can often make this go away by also deriving `Database.PostgreSQL.Simple.ToField.ToPgField SomeType`.

The reason for this error is that Hpgsql needs to know the type's OID, and the interface of `ToField` from postgresql-simple doesn't require implementations to provide one.

### Errors related to Integer sizes

Hpgsql uses PostgreSQL's binary protocol, where the representation differs across different `Int` sizes, like `Int32` and `Int64`.
Hpgsql can decode into Haskell's `Int` as long as the source value is the same size or smaller, but when encoding you might need to use `Int32` more often, since most hardware out there makes an `Int` an `Int64`, and we more commonly use `integer` (32 bit) in PostgreSQL.
