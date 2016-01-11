# Error types

Very basic unified error types are provided, to help distinguish between user
errors, programmer errors, modeller errors and errors of unknown responsibility.

These contain codes which can be matched by the client-side tools. In theory,
all errors returned from GSSA (and many from GSSF) will be one of these. Any
errors thrown beneath will be caught and wrapped accordingly.

These are passed back across WAMP to the client, which is why we do not wish to
pass arbitrary exceptions from Python server-side routines.

Error Code   | Error ID | Intepretation | GSSF exception class (if app.)
-------------|----------|---------------|---------------------
`SUCCESS`    | 0        | All worked | -
`E_UNKNOWN`  | 1        | Error of unknown origin | `GoSmartError`
`E_CLIENT`   | 2        | Triggered by an issue on the client side, such as illogical input | `GoSmartClientError`
`E_SERVER`   | 3        | Problems with the server or server-side tools | `GoSmartServerError`
`E_MODEL`    | 4        | Modelling problem, where the server cannot complete the task for physical/mathematical/numerical/syntactical reasons that are the responsibility of the model developer | `GoSmartModelError`

In general, we err on the side of caution and attribute anything uncertain to
`E_SERVER` or `E_UNKNOWN`. However, it may be, in the future, that being less
conservative with `E_MODEL` will help provide automatic feedback on issues.
