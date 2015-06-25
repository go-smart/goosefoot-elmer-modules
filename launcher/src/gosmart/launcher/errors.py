from gosmart.server.error import Error


class GoSmartError(Exception):
    id = Error["E_UNKNOWN"]
    code = "E_UNKNOWN"


class GoSmartClientError(GoSmartError):
    id = Error["E_CLIENT"]
    code = "E_CLIENT"


class GoSmartServerError(GoSmartError):
    id = Error["E_SERVER"]
    code = "E_SERVER"


class GoSmartModelError(GoSmartError):
    id = Error["E_MODEL"]
    code = "E_MODEL"

exceptions = dict((e.code, e) for e in [
    GoSmartError,
    GoSmartClientError,
    GoSmartServerError,
    GoSmartModelError
])
