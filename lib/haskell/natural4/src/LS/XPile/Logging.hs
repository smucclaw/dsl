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

module LS.XPile.Logging (XPileLog, XPileLogE, xpLog, mutter, mutters, xpReturn, xpError) where

import Control.Monad.RWS ( evalRWS, MonadWriter(tell), RWS
                         , evalRWST, RWST )
import Data.Map as Map ( Map )

-- | typical usage (see `LS.XPile.Purescript.translate2PS` for an example):
--
-- @module Transpiler@ defines an @asOutput :: XPileLog T.Text@
--
-- the @asOoutput@ function can @tell [String]@
--
-- and it returns its output using pure or returne
--
-- the reader and state usually go unused, but if you want
-- to jot down your scribbles, you can use a Map String String
-- or define your own type along these lines
type XPileLog = RWS
                (Map String String) -- ^ reader
                [String]            -- ^ writer
                (Map String String) -- ^ state

-- | This library supports two major modes of logging. In the course of
-- normal operation, you can stream output to the equivalent of STDERR by
-- calling `mutter`; this is a @tell@, and the caller can take stock
-- at the end and output all the mutterings to the timestap.err file.
-- And if there are warnings along the way you could use this
-- mechanism. In the future a more sophisticated Writer could even
-- distinguish different loglevels.

mutter :: String -> XPileLog ()
mutter s = tell [s]

mutters :: [String] -> XPileLog ()
mutters = tell

-- | But if there is a need to throw an unrecoverable error, then
-- return a Left value, by using `xpLeft`. And that error will appear
-- in the actual output file, commented.
--
-- Normal output then gets returned via `xpReturn`.
type XPileLogE a = XPileLog (Either String a)
                
-- | the caller calls @(output, err) = xpLog asOutput@
--
-- The err is a list of strings morally equivalent to STDERR
-- the output is passed through.
--
-- This function is usually called in `app/Main.hs`
xpLog :: XPileLog a -> (a, [String])
xpLog x = evalRWS x mempty mempty

xpReturn, xpRight :: a -> XPileLogE a
xpReturn = xpRight
xpRight = pure . Right

xpError, xpLeft :: String -> XPileLogE a
xpError = xpLeft
xpLeft = pure . Left


