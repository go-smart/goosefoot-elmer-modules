from enum import Enum


class Error(Enum):
    SUCCESS, E_UNKNOWN, E_CLIENT, E_SERVER, E_MODEL, IN_PROGRESS = range(6)


def makeError(ref, message):
    id = ref if isinstance(ref, int) else Error[ref].value
    code = Error(ref).name if isinstance(ref, int) else ref

    return {'id': id, 'code': code, 'message': message}
