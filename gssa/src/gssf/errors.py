from gssa.error import Error


# This is the default, catch-all error - E_UNKNOWN
class GoSmartError(Exception):
    id = Error["E_UNKNOWN"]
    code = "E_UNKNOWN"


# If we have reason to believe the wrapped error is the client's responsibility
class GoSmartClientError(GoSmartError):
    id = Error["E_CLIENT"]
    code = "E_CLIENT"


# If we think this is a problem on this side of the fence
class GoSmartServerError(GoSmartError):
    id = Error["E_SERVER"]
    code = "E_SERVER"


# If we can pinpoint this as a problem with the numerical model (however, we err
# on the side of caution and attribute to server if not sure - perhaps this
# should change depending on how that feedback is used)
class GoSmartModelError(GoSmartError):
    id = Error["E_MODEL"]
    code = "E_MODEL"

exceptions = dict((e.code, e) for e in [
    GoSmartError,
    GoSmartClientError,
    GoSmartServerError,
    GoSmartModelError
])
