{-| A module for wrapping transpilation errors and STDERR trace mumbling.

This section explains the developer motivation for this library.

* Users want to mutter: incidental logging

Sometimes it's useful to do a bit of "printf debugging". "What is
going on here? Let's dump it somehow, and inspect it on the next run."

If you're in IO, easy: just @print@ it. With a bit more discipline, actually, @hPutStrLn stderr@ it.

If you're not in IO? Well, that's why they invented @Debug.Trace@.

But is that the Right Way? No. The Right Way would provide some
mechanism to @mutter@ a bit of output, and that output would be
gracefully and cleanly handled by the rest of the program.

For instance, if we are producing ten output files named @0.out@
through @9.out@, maybe the incidental mutters should be streamed
elegantly to companion files @0.err@ through @9.err@.

@putStrLn@ and @trace@ won't do that for you.

The pattern we want is essentially the Writer monad. We could alias
@mutter = tell@ and wrap all our mumbly little computations in that
monad, and we would have most of the functionality we want.

Sadly, a naive Writer monad is discouraged by the gurus because of
inefficiencies around list appending and things to do with strictness.
And that casts a pall on the whole RWS family of monad transformers.

That's why other modules were invented:

- monad-logger
- fast-logger
- validation (Data.Valiation)
- monad-validate (Control.Monad.Validate)

Those last two do another thing that users want.

* Users want to throw: fatal errors

The above mutterings provide a mechanism for logging warnings.
Warnings aren't severe enough to prematurely terminate the
computation, but they're worth heeding nonetheless.

Fatal errors do terminate the computation. We want to bubble those
errors back up the call stack so the top-level caller can decide how
to handle them.

The solution presented by most introductory tutorials is an @Either@,
which puts errors on the @Left@ and happy output on the @Right@.
@Either@ implements Control.Monad.Except, which provides the
@MonadError@ class with a `throwError` method.

Naively, one might want to wrap an RWS around an Either, so as to be
able to @tell@ both log warnings and incidental mutterings, and
@throwError@ fatal errors distinguished from desired output.

The current (first-draft) version of this module does that. In future
we may switch to some combination of ValidateT and monad-chronicle or
monad-logger.

For gory details around @RWST (Either String) a@ versus @RWS (Either String a)@, see
https://gist.github.com/mengwong/73af81ad600a533f12ef42fc655fed0f

-}

module LS.XPile.Logging
  ( XPileLog,
    XPileLogE,
    xpLog,
    mutter,
    mutters,
    xpReturn,
    xpError,
    XPileLogW
                        , fmapE ,
    fromxpLogE
  )
where

import Data.Bifunctor (second)
import Control.Monad.RWS
  ( MonadWriter (tell),
    RWS,
    RWST,
    evalRWS,
    evalRWST,
  )
import Data.Either (fromRight)
import Data.HashMap.Strict as Map (HashMap)
import Flow ((|>))

-- | typical usage
--
-- the caller (e.g. @app/Main@) calls @(output, err) = xpLog asOutput@.
--
-- In the tuple, we find a list of strings in @err@.
--
-- In the simple case, the @asOutput@ could be simple: @output@ is a @String@.
-- (see `LS.XPile.Purescript.translate2PS` for an example).
--
xpLog :: XPileLog a -> (a, XPileLogW)
xpLog x = evalRWS x mempty mempty

-- | In a more complex case, @output@ is an @Either [String] String@
-- with @MonadError@ semantics.
-- (see `LS.XPile.CoreL4.sfl4ToCoreL4` for an example).
-- 
-- To set that up, we use `XPileLogE` instead of `XPileLog`. The @E@
-- stands for @Either@. We deliberately use the same type for the
-- Either Left as for the stderr stream. So the Either Left can return
-- a list of errors; this anticipates monad-validate.
type XPileLogE a = XPileLog (Either XPileLogW a)

-- * The underlying types are not exported by this module, except XPileLogW.
--
-- the reader and state usually go unused, but we set them to String
-- in case you need that. You can also define your own type along
-- these lines.
type XPileLog  = RWS XPileLogR XPileLogW XPileLogS
type XPileLogR = HashMap String String
type XPileLogW = [XPileLogW'];       type XPileLogW' = String
type XPileLogS = HashMap String String

-- | This library supports two major modes of logging. In the course
-- of normal operation, you can stream to the equivalent of STDERR by
-- calling `mutter`; this is a @tell@, and the caller can take stock
-- at the end and output all the mutterings to the timestap.err file.
-- And if there are warnings along the way you could use this
-- mechanism. In the future a more sophisticated Writer could even
-- distinguish different loglevels. For now, use this for printf
-- debugging.

mutter :: XPileLogW' -> XPileLog ()
mutter = tell . pure

-- | use `mutter` for single and `mutters` for plural muttering
mutters :: XPileLogW -> XPileLog ()
mutters = tell

-- | But if there is a need to throw an unrecoverable error, then
-- return a Left value, by using `xpError`. And that error will appear
-- in the actual output file, commented.

xpError, xpLeft :: XPileLogW -> XPileLogE a
xpError = xpLeft

-- | xpLeft is the underlying mechanism, private to this module, so
-- can be swapped out if one day we change the underlying.
xpLeft  = pure . Left

-- | Normal output then gets returned via `xpReturn`.
xpReturn, xpRight :: a -> XPileLogE a
xpReturn = xpRight

-- | xpRight is the underlying mechanism for xpReturn.
xpRight = pure . Right

-- | fmap over the right value of an XPileLogE
fmapE :: (a -> a) -> XPileLogE a -> XPileLogE a
fmapE f = fmap (second f)

fromxpLogE :: Monoid a => XPileLogE a -> a
fromxpLogE xpLogE = xpLogE |> xpLog |> fst |> fromRight mempty